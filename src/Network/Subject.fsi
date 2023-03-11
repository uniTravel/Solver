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
    /// <remarks>问题建模验证规则：
    /// <para>1、根节点供应量为零。</para>
    /// <para>2、节点供应量总和为零。</para>
    /// <para>3、以弧成本字典为准，节点邻接表的邻接关系与之一致。</para>
    /// <para>4、根节点没有弧相连。</para>
    /// <para>5、除根节点之外的图为连通图。</para>
    /// </remarks>
    /// <param name="n">节点供应量数组：数组索引作为节点编号，第一个元素作为根节点。</param>
    /// <param name="adj">节点邻接列表。</param>
    /// <param name="inv">节点逆邻接列表。</param>
    /// <param name="cost">弧成本字典。</param>
    /// <param name="capacity">弧容量上界字典：无上界约束的弧不加入字典，容量下界为零。</param>
    /// <returns>网络规划问题</returns>
    val init:
        n: int[] ->
        adj: int list list ->
        inv: int list list ->
        cost: IDictionary<int * int, int> ->
        capacity: IDictionary<int * int, int> ->
            T

    /// <summary>获取网络规划问题值
    /// </summary>
    /// <param name="sub">网络规划问题</param>
    /// <returns>网络规划问题值</returns>
    val value: sub: T -> int[] * IDictionary<int * int, int> * IDictionary<int * int, int>
