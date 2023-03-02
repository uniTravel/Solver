namespace Solver.Network

open System.Collections.Generic


/// <summary>网络规划问题的解
/// </summary>
/// <typeparam name="Optimal">最优解，包括生成树解以及达到上界流量的弧。</typeparam>
/// <typeparam name="Feasible">可行解。</typeparam>
/// <typeparam name="Infeasible">问题不可行。</typeparam>
/// <typeparam name="Unbounded">问题无界。</typeparam>
type Solution =
    | Optimal of Dictionary<int * int, int> * Dictionary<int * int, int>
    | Feasible
    | Infeasible
    | Unbounded

/// <summary>网络规划问题模块
/// </summary>
[<RequireQualifiedAccess>]
module Subject =

    /// <summary>网络规划问题类型
    /// </summary>
    type T

    /// <summary>创建网络规划问题
    /// </summary>
    /// <param name="n">节点及其供应量：索引为零的系备用根节点，其他节点应按值倒序排列。</param>
    /// <param name="cost">弧成本列表：弧尾*弧头*成本。</param>
    /// <param name="capacity">弧容量上界字典：无上界约束的弧不加入字典，容量下界为零。</param>
    /// <returns>网络规划问题</returns>
    val init: n: int[] -> cost: (int * int * int) list -> capacity: IDictionary<int * int, int> -> T

    /// <summary>获取网络规划问题值
    /// </summary>
    /// <param name="sub">网络规划问题</param>
    /// <returns>网络规划问题值</returns>
    val value: sub: T -> int[] * (int * int * int) list * IDictionary<int * int, int>
