"""Shared statistics helpers: roots as biological replicates.

Cells imaged in the same root are not independent, so comparisons of cell-level
measurements use linear mixed models with a random intercept per root
(statsmodels MixedLM, REML). Roots are assigned from image file names with
``rootid.root_id``. Two-group comparisons are also given an exact permutation test
that shuffles the condition labels between roots (on root medians).
"""
import itertools
import math
import warnings

import numpy as np
import statsmodels.formula.api as smf
from statsmodels.tools.sm_exceptions import ConvergenceWarning

from rootid import root_id  # noqa: F401  (re-exported for the figure scripts)

warnings.simplefilter("ignore", ConvergenceWarning)


def holm(p_values):
    ps = list(p_values)
    order = np.argsort(ps)
    adjusted = [0.0] * len(ps)
    running = 0.0
    for rank, i in enumerate(order):
        running = max(running, min(1.0, (len(ps) - rank) * ps[i]))
        adjusted[i] = running
    return adjusted


def fit_mixed(df, formula, groups="root"):
    """REML fit with a random intercept per ``groups``. Several optimisers are tried and the
    fit with the highest finite log-likelihood and finite standard errors is kept (L-BFGS
    alone can stall at a degenerate solution when the root variance is close to zero)."""
    model = smf.mixedlm(formula, df, groups=df[groups])
    best = None
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        for method in ("bfgs", "powell", "nm", "lbfgs"):
            try:
                res = model.fit(reml=True, method=method)
            except Exception:
                continue
            if np.isfinite(res.llf) and np.all(np.isfinite(res.bse_fe)) and (best is None or res.llf > best.llf + 1e-9):
                best = res
    if best is None:
        raise RuntimeError(f"mixed model did not converge: {formula}")
    return best


def contrast(res, weights):
    """Wald test of a linear combination of fixed effects. ``weights``: {term: coefficient}."""
    names = list(res.fe_params.index)
    L = np.zeros((1, len(names)))
    for term, w in weights.items():
        L[0, names.index(term)] = w
    t = res.t_test(L)
    return float(np.squeeze(t.effect)), float(np.squeeze(t.sd)), float(np.squeeze(t.pvalue))


def group_contrast_vector(res, groups, a, b, term="C(group)"):
    """Contrast weights for mean(b) - mean(a) in a model ``y ~ C(group)`` (treatment coding)."""
    w = {}
    for g, sign in ((b, 1.0), (a, -1.0)):
        if g != groups[0]:
            key = f"{term}[T.{g}]"
            w[key] = w.get(key, 0.0) + sign
    return w


def pairwise_groups(res, groups, term="C(group)"):
    """All pairwise differences between groups of a ``y ~ C(group)`` model, Holm-adjusted."""
    rows = []
    for a, b in itertools.combinations(groups, 2):
        w = group_contrast_vector(res, groups, a, b, term)
        if not w:
            continue
        est, se, p = contrast(res, w)
        rows.append({"group_a": a, "group_b": b, "diff": est, "se": se, "p": p})
    for row, adj in zip(rows, holm([r["p"] for r in rows])):
        row["p_holm"] = adj
    return rows


def exact_root_permutation(control_values, mutant_values, max_enumerate=200_000, n_random=100_000, seed=1):
    """Two-sided permutation p for the difference in means of root-level values; the
    condition labels are shuffled between roots (exact when the number of
    relabellings is small enough). Returns (p, number of relabellings)."""
    a = np.asarray(control_values, float)
    b = np.asarray(mutant_values, float)
    pooled = np.concatenate([a, b])
    n, k = len(pooled), len(b)
    observed = abs(b.mean() - a.mean())
    total = pooled.sum()
    n_relabel = math.comb(n, k)
    if n_relabel <= max_enumerate:
        sums = np.array([pooled[list(c)].sum() for c in itertools.combinations(range(n), k)])
    else:
        rng = np.random.default_rng(seed)
        sums = np.array([pooled[rng.permutation(n)[:k]].sum() for _ in range(n_random)])
    stats = np.abs(sums / k - (total - sums) / (n - k))
    return float(np.mean(stats >= observed - 1e-12)), n_relabel


def compact_letters(groups, significant):
    """Compact letter display (insert-absorb). ``significant[(a, b)]`` is True when a and b
    differ; groups that share a letter do not differ."""
    sets = [set(groups)]
    for a, b in itertools.combinations(groups, 2):
        if not (significant.get((a, b)) or significant.get((b, a))):
            continue
        new = []
        for s in sets:
            new += [s - {a}, s - {b}] if (a in s and b in s) else [s]
        new = [s for s in new if s]
        new = [s for s in new if not any(s < t for t in new)]
        sets = [s for i, s in enumerate(new) if s not in new[:i]]
    # order letters by first appearance along ``groups``
    sets.sort(key=lambda s: min(groups.index(g) for g in s))
    labels = {g: "" for g in groups}
    for i, s in enumerate(sets):
        for g in s:
            labels[g] += "abcdefghijklmnopqrstuvwxyz"[i]
    return labels
