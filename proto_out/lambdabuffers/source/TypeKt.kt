//Generated by the protocol buffer compiler. DO NOT EDIT!
// source: repls/lambdabuffers-source.proto

package lambdabuffers.source;

@kotlin.jvm.JvmSynthetic
public inline fun type(block: lambdabuffers.source.TypeKt.Dsl.() -> kotlin.Unit): lambdabuffers.source.LambdabuffersSource.Type =
  lambdabuffers.source.TypeKt.Dsl._create(lambdabuffers.source.LambdabuffersSource.Type.newBuilder()).apply { block() }._build()
public object TypeKt {
  @kotlin.OptIn(com.google.protobuf.kotlin.OnlyForUseByGeneratedProtoCode::class)
  @com.google.protobuf.kotlin.ProtoDslMarker
  public class Dsl private constructor(
    private val _builder: lambdabuffers.source.LambdabuffersSource.Type.Builder
  ) {
    public companion object {
      @kotlin.jvm.JvmSynthetic
      @kotlin.PublishedApi
      internal fun _create(builder: lambdabuffers.source.LambdabuffersSource.Type.Builder): Dsl = Dsl(builder)
    }

    @kotlin.jvm.JvmSynthetic
    @kotlin.PublishedApi
    internal fun _build(): lambdabuffers.source.LambdabuffersSource.Type = _builder.build()

    /**
     * <code>.lambdabuffers.source.TypeVar type_var = 1;</code>
     */
    public var typeVar: lambdabuffers.source.LambdabuffersSource.TypeVar
      @JvmName("getTypeVar")
      get() = _builder.getTypeVar()
      @JvmName("setTypeVar")
      set(value) {
        _builder.setTypeVar(value)
      }
    /**
     * <code>.lambdabuffers.source.TypeVar type_var = 1;</code>
     */
    public fun clearTypeVar() {
      _builder.clearTypeVar()
    }
    /**
     * <code>.lambdabuffers.source.TypeVar type_var = 1;</code>
     * @return Whether the typeVar field is set.
     */
    public fun hasTypeVar(): kotlin.Boolean {
      return _builder.hasTypeVar()
    }

    /**
     * <code>.lambdabuffers.source.TypeCon type_con = 2;</code>
     */
    public var typeCon: lambdabuffers.source.LambdabuffersSource.TypeCon
      @JvmName("getTypeCon")
      get() = _builder.getTypeCon()
      @JvmName("setTypeCon")
      set(value) {
        _builder.setTypeCon(value)
      }
    /**
     * <code>.lambdabuffers.source.TypeCon type_con = 2;</code>
     */
    public fun clearTypeCon() {
      _builder.clearTypeCon()
    }
    /**
     * <code>.lambdabuffers.source.TypeCon type_con = 2;</code>
     * @return Whether the typeCon field is set.
     */
    public fun hasTypeCon(): kotlin.Boolean {
      return _builder.hasTypeCon()
    }

    /**
     * <code>.lambdabuffers.source.TypeApp type_app = 3;</code>
     */
    public var typeApp: lambdabuffers.source.LambdabuffersSource.TypeApp
      @JvmName("getTypeApp")
      get() = _builder.getTypeApp()
      @JvmName("setTypeApp")
      set(value) {
        _builder.setTypeApp(value)
      }
    /**
     * <code>.lambdabuffers.source.TypeApp type_app = 3;</code>
     */
    public fun clearTypeApp() {
      _builder.clearTypeApp()
    }
    /**
     * <code>.lambdabuffers.source.TypeApp type_app = 3;</code>
     * @return Whether the typeApp field is set.
     */
    public fun hasTypeApp(): kotlin.Boolean {
      return _builder.hasTypeApp()
    }

    /**
     * <code>.lambdabuffers.source.TypeRef type_ref = 4;</code>
     */
    public var typeRef: lambdabuffers.source.LambdabuffersSource.TypeRef
      @JvmName("getTypeRef")
      get() = _builder.getTypeRef()
      @JvmName("setTypeRef")
      set(value) {
        _builder.setTypeRef(value)
      }
    /**
     * <code>.lambdabuffers.source.TypeRef type_ref = 4;</code>
     */
    public fun clearTypeRef() {
      _builder.clearTypeRef()
    }
    /**
     * <code>.lambdabuffers.source.TypeRef type_ref = 4;</code>
     * @return Whether the typeRef field is set.
     */
    public fun hasTypeRef(): kotlin.Boolean {
      return _builder.hasTypeRef()
    }
    public val typeCase: lambdabuffers.source.LambdabuffersSource.Type.TypeCase
      @JvmName("getTypeCase")
      get() = _builder.getTypeCase()

    public fun clearType() {
      _builder.clearType()
    }
  }
}
@kotlin.jvm.JvmSynthetic
public inline fun lambdabuffers.source.LambdabuffersSource.Type.copy(block: lambdabuffers.source.TypeKt.Dsl.() -> kotlin.Unit): lambdabuffers.source.LambdabuffersSource.Type =
  lambdabuffers.source.TypeKt.Dsl._create(this.toBuilder()).apply { block() }._build()
