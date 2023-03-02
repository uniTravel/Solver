namespace Solver.Network


/// <summary>网络规划原算法模块
/// </summary>
[<RequireQualifiedAccess>]
module Primal =

    /// <summary>网络规划原算法类型
    /// </summary>
    type T

    /// <summary>创建网络规划原算法模型
    /// </summary>
    /// <param name="sub">网络规划问题。</param>
    /// <returns>网络规划原算法模型</returns>
    val create: sub: Subject.T -> T


    /// <summary>求解网络规划问题
    /// </summary>
    /// <param name="size">入基候选字典的最大长度。</param>
    /// <param name="sub">网络规划原算法模型。</param>
    /// <returns>网络规划问题的解</returns>
    val solve: size: int -> sub: T -> Solution
