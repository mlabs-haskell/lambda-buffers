//Generated by the protocol buffer compiler. DO NOT EDIT!
// source: repls/lambdabuffers-source.proto

package lambdabuffers.source;

@kotlin.jvm.JvmSynthetic
public inline fun typeCon(block: lambdabuffers.source.TypeConKt.Dsl.() -> kotlin.Unit): lambdabuffers.source.LambdabuffersSource.TypeCon =
  lambdabuffers.source.TypeConKt.Dsl._create(lambdabuffers.source.LambdabuffersSource.TypeCon.newBuilder()).apply { block() }._build()
public object TypeConKt {
  @kotlin.OptIn(com.google.protobuf.kotlin.OnlyForUseByGeneratedProtoCode::class)
  @com.google.protobuf.kotlin.ProtoDslMarker
  public class Dsl private constructor(
    private val _builder: lambdabuffers.source.LambdabuffersSource.TypeCon.Builder
  ) {
    public companion object {
      @kotlin.jvm.JvmSynthetic
      @kotlin.PublishedApi
      internal fun _create(builder: lambdabuffers.source.LambdabuffersSource.TypeCon.Builder): Dsl = Dsl(builder)
    }

    @kotlin.jvm.JvmSynthetic
    @kotlin.PublishedApi
    internal fun _build(): lambdabuffers.source.LambdabuffersSource.TypeCon = _builder.build()

    /**
     * <code>.lambdabuffers.source.Kind kind = 1;</code>
     */
    public var kind: lambdabuffers.source.LambdabuffersSource.Kind
      @JvmName("getKind")
      get() = _builder.getKind()
      @JvmName("setKind")
      set(value) {
        _builder.setKind(value)
      }
    /**
     * <code>.lambdabuffers.source.Kind kind = 1;</code>
     */
    public fun clearKind() {
      _builder.clearKind()
    }
    /**
     * <code>.lambdabuffers.source.Kind kind = 1;</code>
     * @return Whether the kind field is set.
     */
    public fun hasKind(): kotlin.Boolean {
      return _builder.hasKind()
    }

    /**
     * <code>.lambdabuffers.source.SourceInfo source_info = 2;</code>
     */
    public var sourceInfo: lambdabuffers.source.LambdabuffersSource.SourceInfo
      @JvmName("getSourceInfo")
      get() = _builder.getSourceInfo()
      @JvmName("setSourceInfo")
      set(value) {
        _builder.setSourceInfo(value)
      }
    /**
     * <code>.lambdabuffers.source.SourceInfo source_info = 2;</code>
     */
    public fun clearSourceInfo() {
      _builder.clearSourceInfo()
    }
    /**
     * <code>.lambdabuffers.source.SourceInfo source_info = 2;</code>
     * @return Whether the sourceInfo field is set.
     */
    public fun hasSourceInfo(): kotlin.Boolean {
      return _builder.hasSourceInfo()
    }
  }
}
@kotlin.jvm.JvmSynthetic
public inline fun lambdabuffers.source.LambdabuffersSource.TypeCon.copy(block: lambdabuffers.source.TypeConKt.Dsl.() -> kotlin.Unit): lambdabuffers.source.LambdabuffersSource.TypeCon =
  lambdabuffers.source.TypeConKt.Dsl._create(this.toBuilder()).apply { block() }._build()
