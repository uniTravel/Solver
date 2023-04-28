namespace Solver.Network


/// <summary>网络规划对偶算法模块
/// </summary>
[<RequireQualifiedAccess>]
module Dual =

    /// <summary>网络规划对偶算法类型
    /// </summary>
    type T

    /// <summary>创建网络规划对偶算法模型
    /// </summary>
    /// <remarks>建模验证规则：
    /// <para>1、所有弧必须包含上界。</para>
    /// </remarks>
    /// <param name="sub">网络规划问题。</param>
    /// <returns>网络规划对偶算法模型</returns>
    val create: sub: Subject.T -> T

    /// <summary>求解网络规划问题
    /// </summary>
    /// <remarks>Primal-Dual方法。</remarks>
    /// <param name="sub">网络规划对偶算法模型。</param>
    /// <returns>网络规划问题的解</returns>
    val pd: sub: T -> Solution

    /// <summary>求解网络规划问题
    /// </summary>
    /// <remarks>Sequencial Shortest Path方法。</remarks>
    /// <param name="sub">网络规划对偶算法模型。</param>
    /// <returns>网络规划问题的解</returns>
    val ssp: sub: T -> Solution

    /// <summary>求解网络规划问题
    /// </summary>
    /// <remarks>Relaxation方法。</remarks>
    /// <param name="sub">网络规划对偶算法模型。</param>
    /// <returns>网络规划问题的解</returns>
    val rex: sub: T -> Solution

    /// <summary>优化网络规划问题
    /// </summary>
    /// <remarks>
    /// <para>1、Single-Node Relaxation迭代。</para>
    /// <para>2、优化后的问题不建议用Relaxation方法求解。</para>
    /// </remarks>
    /// <param name="sub">网络规划对偶算法模型。</param>
    /// <returns>优化后的网络规划对偶算法模型。</returns>
    val snr: sub: T -> T
