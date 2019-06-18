let ocaml =
  ( __LINE__ + 1
  , {|


  let loop : (('a -> 'b) -> ('a -> 'b)) -> ('a -> 'b);

  let read_lines : string -> list(string);

  module Request : {
    type t('a) = ..;

    type req('a) = t('a);

    module Response : {
      type t('a) = Provide('a) | Delegate(req('a)) | Unhandled;
    };

    type response = ..;
  };

  module Var : {
    type t;

    let (>=) : t -> t -> bool;

    let (<=) : t -> t -> bool;

    let (=) : t -> t -> bool;

    let (>) : t -> t -> bool;

    let (<) : t -> t -> bool;

    let (<>) : t -> t -> bool;

    let equal : t -> t -> bool;

    let compare : t -> t -> int;

    let min : t -> t -> t;

    let max : t -> t -> t;

    let ascending : t -> t -> int;

    let descending : t -> t -> int;

    let between : t -> low:t -> high:t -> bool;

    let clamp_exn : t -> min:t -> max:t -> t;

    let create : int -> t;
  };

  module Constraint : {
    type t =
      | Boolean(field_var)
      | Equal(field_var, field_var)
      | Square(field_var, field_var)
      | R1CS(field_var, field_var);

    let boolean : ?label:string -> field_var -> t;

    let equal : ?label:string -> field_var -> field_var -> t;

    let r1cs : ?label:string -> field_var -> field_var -> field_var -> t;

    let square : ?label:string -> field_var -> field_var -> t;
  };

  module Typ : {
    module Store : {
      type t('a);

      let bind : t('a) -> f:('a -> t('b)) -> t('b);

      let return : 'a -> t('a);

      let map : t('a) -> f:('a -> 'b) -> t('b);

      let store : field -> t(field_var);
    };

    module Alloc : {
      type t('a);

      let bind : t('a) -> f:('a -> t('b)) -> t('b);

      let return : 'a -> t('a);

      let map : t('a) -> f:('a -> 'b) -> t('b);

      let alloc : t(field_var);
    };

    module Read : {
      type t('a);

      let bind : t('a) -> f:('a -> t('b)) -> t('b);

      let return : 'a -> t('a);

      let map : t('a) -> f:('a -> 'b) -> t('b);

      let read : field_var -> t(field);
    };

    /* A version of Checked.t exposed to support the Typ.t below.
       This is not available to any user code. */
    type checked_secret('a);

    type t('var, 'value) = {
      store: 'value -> Store.t('var),
      read: 'var -> Read.t('value),
      alloc: Alloc.t('var),
      check: 'var -> checked_secret(unit)
    };

    let store : {t('var, 'value)} -> 'value -> Store.t('var);

    let read : {t('var, 'value)} -> 'var -> Read.t('value);

    let alloc : {t('var, 'value)} -> Alloc.t('var);

    instance check : {t('var, 'value)} -> 'var -> unit;

    instance unit : t(unit, unit);

    let field : t(field_var, field);

    let tuple2 :
         {t('var1, 'value1)}
      -> {t('var2, 'value2)}
      -> t(('var1, 'var2), ('value1, 'value2));

    instance ( * ) :
         {t('var1, 'value1)}
      -> {t('var2, 'value2)}
      -> t(('var1, 'var2), ('value1, 'value2));

    instance tuple3 :
         {t('var1, 'value1)}
      -> {t('var2, 'value2)}
      -> {t('var3, 'value3)}
      -> t(('var1, 'var2, 'var3), ('value1, 'value2, 'value3));

    let list :
      length:int -> {t('var, 'value)} -> t(list('var), list('value));

    instance list0 : {t('var, 'value)} -> t(list#0('var), list#0('value));
    instance list1 : {t('var, 'value)} -> t(list#1('var), list#1('value));
    instance list2 : {t('var, 'value)} -> t(list#2('var), list#2('value));
    instance list3 : {t('var, 'value)} -> t(list#3('var), list#3('value));
    instance list4 : {t('var, 'value)} -> t(list#4('var), list#4('value));
    instance list5 : {t('var, 'value)} -> t(list#5('var), list#5('value));
    instance list6 : {t('var, 'value)} -> t(list#6('var), list#6('value));
    instance list7 : {t('var, 'value)} -> t(list#7('var), list#7('value));
    instance list8 : {t('var, 'value)} -> t(list#8('var), list#8('value));
    instance list9 : {t('var, 'value)} -> t(list#9('var), list#9('value));
    instance list10 : {t('var, 'value)} -> t(list#10('var), list#10('value));
    instance list11 : {t('var, 'value)} -> t(list#11('var), list#11('value));
    instance list12 : {t('var, 'value)} -> t(list#12('var), list#12('value));
    instance list13 : {t('var, 'value)} -> t(list#13('var), list#13('value));
    instance list14 : {t('var, 'value)} -> t(list#14('var), list#14('value));
    instance list15 : {t('var, 'value)} -> t(list#15('var), list#15('value));
    instance list16 : {t('var, 'value)} -> t(list#16('var), list#16('value));
    instance list17 : {t('var, 'value)} -> t(list#17('var), list#17('value));
    instance list18 : {t('var, 'value)} -> t(list#18('var), list#18('value));
    instance list19 : {t('var, 'value)} -> t(list#19('var), list#19('value));
    instance list20 : {t('var, 'value)} -> t(list#20('var), list#20('value));
    instance list21 : {t('var, 'value)} -> t(list#21('var), list#21('value));
    instance list22 : {t('var, 'value)} -> t(list#22('var), list#22('value));
    instance list23 : {t('var, 'value)} -> t(list#23('var), list#23('value));
    instance list24 : {t('var, 'value)} -> t(list#24('var), list#24('value));
    instance list25 : {t('var, 'value)} -> t(list#25('var), list#25('value));
    instance list26 : {t('var, 'value)} -> t(list#26('var), list#26('value));
    instance list27 : {t('var, 'value)} -> t(list#27('var), list#27('value));
    instance list28 : {t('var, 'value)} -> t(list#28('var), list#28('value));
    instance list29 : {t('var, 'value)} -> t(list#29('var), list#29('value));
    instance list30 : {t('var, 'value)} -> t(list#30('var), list#30('value));
    instance list31 : {t('var, 'value)} -> t(list#31('var), list#31('value));
    instance list32 : {t('var, 'value)} -> t(list#32('var), list#32('value));
    instance list33 : {t('var, 'value)} -> t(list#33('var), list#33('value));
    instance list34 : {t('var, 'value)} -> t(list#34('var), list#34('value));
    instance list35 : {t('var, 'value)} -> t(list#35('var), list#35('value));
    instance list36 : {t('var, 'value)} -> t(list#36('var), list#36('value));
    instance list37 : {t('var, 'value)} -> t(list#37('var), list#37('value));
    instance list38 : {t('var, 'value)} -> t(list#38('var), list#38('value));
    instance list39 : {t('var, 'value)} -> t(list#39('var), list#39('value));
    instance list40 : {t('var, 'value)} -> t(list#40('var), list#40('value));
    instance list41 : {t('var, 'value)} -> t(list#41('var), list#41('value));
    instance list42 : {t('var, 'value)} -> t(list#42('var), list#42('value));
    instance list43 : {t('var, 'value)} -> t(list#43('var), list#43('value));
    instance list44 : {t('var, 'value)} -> t(list#44('var), list#44('value));
    instance list45 : {t('var, 'value)} -> t(list#45('var), list#45('value));
    instance list46 : {t('var, 'value)} -> t(list#46('var), list#46('value));
    instance list47 : {t('var, 'value)} -> t(list#47('var), list#47('value));
    instance list48 : {t('var, 'value)} -> t(list#48('var), list#48('value));
    instance list49 : {t('var, 'value)} -> t(list#49('var), list#49('value));
    instance list50 : {t('var, 'value)} -> t(list#50('var), list#50('value));
    instance list51 : {t('var, 'value)} -> t(list#51('var), list#51('value));
    instance list52 : {t('var, 'value)} -> t(list#52('var), list#52('value));
    instance list53 : {t('var, 'value)} -> t(list#53('var), list#53('value));
    instance list54 : {t('var, 'value)} -> t(list#54('var), list#54('value));
    instance list55 : {t('var, 'value)} -> t(list#55('var), list#55('value));
    instance list56 : {t('var, 'value)} -> t(list#56('var), list#56('value));
    instance list57 : {t('var, 'value)} -> t(list#57('var), list#57('value));
    instance list58 : {t('var, 'value)} -> t(list#58('var), list#58('value));
    instance list59 : {t('var, 'value)} -> t(list#59('var), list#59('value));
    instance list60 : {t('var, 'value)} -> t(list#60('var), list#60('value));
    instance list61 : {t('var, 'value)} -> t(list#61('var), list#61('value));
    instance list62 : {t('var, 'value)} -> t(list#62('var), list#62('value));
    instance list63 : {t('var, 'value)} -> t(list#63('var), list#63('value));
    instance list64 : {t('var, 'value)} -> t(list#64('var), list#64('value));
    instance list65 : {t('var, 'value)} -> t(list#65('var), list#65('value));
    instance list66 : {t('var, 'value)} -> t(list#66('var), list#66('value));
    instance list67 : {t('var, 'value)} -> t(list#67('var), list#67('value));
    instance list68 : {t('var, 'value)} -> t(list#68('var), list#68('value));
    instance list69 : {t('var, 'value)} -> t(list#69('var), list#69('value));
    instance list70 : {t('var, 'value)} -> t(list#70('var), list#70('value));
    instance list71 : {t('var, 'value)} -> t(list#71('var), list#71('value));
    instance list72 : {t('var, 'value)} -> t(list#72('var), list#72('value));
    instance list73 : {t('var, 'value)} -> t(list#73('var), list#73('value));
    instance list74 : {t('var, 'value)} -> t(list#74('var), list#74('value));
    instance list75 : {t('var, 'value)} -> t(list#75('var), list#75('value));
    instance list76 : {t('var, 'value)} -> t(list#76('var), list#76('value));
    instance list77 : {t('var, 'value)} -> t(list#77('var), list#77('value));
    instance list78 : {t('var, 'value)} -> t(list#78('var), list#78('value));
    instance list79 : {t('var, 'value)} -> t(list#79('var), list#79('value));
    instance list80 : {t('var, 'value)} -> t(list#80('var), list#80('value));
    instance list81 : {t('var, 'value)} -> t(list#81('var), list#81('value));
    instance list82 : {t('var, 'value)} -> t(list#82('var), list#82('value));
    instance list83 : {t('var, 'value)} -> t(list#83('var), list#83('value));
    instance list84 : {t('var, 'value)} -> t(list#84('var), list#84('value));
    instance list85 : {t('var, 'value)} -> t(list#85('var), list#85('value));
    instance list86 : {t('var, 'value)} -> t(list#86('var), list#86('value));
    instance list87 : {t('var, 'value)} -> t(list#87('var), list#87('value));
    instance list88 : {t('var, 'value)} -> t(list#88('var), list#88('value));
    instance list89 : {t('var, 'value)} -> t(list#89('var), list#89('value));
    instance list90 : {t('var, 'value)} -> t(list#90('var), list#90('value));
    instance list91 : {t('var, 'value)} -> t(list#91('var), list#91('value));
    instance list92 : {t('var, 'value)} -> t(list#92('var), list#92('value));
    instance list93 : {t('var, 'value)} -> t(list#93('var), list#93('value));
    instance list94 : {t('var, 'value)} -> t(list#94('var), list#94('value));
    instance list95 : {t('var, 'value)} -> t(list#95('var), list#95('value));
    instance list96 : {t('var, 'value)} -> t(list#96('var), list#96('value));
    instance list97 : {t('var, 'value)} -> t(list#97('var), list#97('value));
    instance list98 : {t('var, 'value)} -> t(list#98('var), list#98('value));
    instance list99 : {t('var, 'value)} -> t(list#99('var), list#99('value));
    instance list100 : {t('var, 'value)} -> t(list#100('var), list#100('value));
    instance list101 : {t('var, 'value)} -> t(list#101('var), list#101('value));
    instance list102 : {t('var, 'value)} -> t(list#102('var), list#102('value));
    instance list103 : {t('var, 'value)} -> t(list#103('var), list#103('value));
    instance list104 : {t('var, 'value)} -> t(list#104('var), list#104('value));
    instance list105 : {t('var, 'value)} -> t(list#105('var), list#105('value));
    instance list106 : {t('var, 'value)} -> t(list#106('var), list#106('value));
    instance list107 : {t('var, 'value)} -> t(list#107('var), list#107('value));
    instance list108 : {t('var, 'value)} -> t(list#108('var), list#108('value));
    instance list109 : {t('var, 'value)} -> t(list#109('var), list#109('value));
    instance list110 : {t('var, 'value)} -> t(list#110('var), list#110('value));
    instance list111 : {t('var, 'value)} -> t(list#111('var), list#111('value));
    instance list112 : {t('var, 'value)} -> t(list#112('var), list#112('value));
    instance list113 : {t('var, 'value)} -> t(list#113('var), list#113('value));
    instance list114 : {t('var, 'value)} -> t(list#114('var), list#114('value));
    instance list115 : {t('var, 'value)} -> t(list#115('var), list#115('value));
    instance list116 : {t('var, 'value)} -> t(list#116('var), list#116('value));
    instance list117 : {t('var, 'value)} -> t(list#117('var), list#117('value));
    instance list118 : {t('var, 'value)} -> t(list#118('var), list#118('value));
    instance list119 : {t('var, 'value)} -> t(list#119('var), list#119('value));
    instance list120 : {t('var, 'value)} -> t(list#120('var), list#120('value));
    instance list121 : {t('var, 'value)} -> t(list#121('var), list#121('value));
    instance list122 : {t('var, 'value)} -> t(list#122('var), list#122('value));
    instance list123 : {t('var, 'value)} -> t(list#123('var), list#123('value));
    instance list124 : {t('var, 'value)} -> t(list#124('var), list#124('value));
    instance list125 : {t('var, 'value)} -> t(list#125('var), list#125('value));
    instance list126 : {t('var, 'value)} -> t(list#126('var), list#126('value));
    instance list127 : {t('var, 'value)} -> t(list#127('var), list#127('value));
    instance list128 : {t('var, 'value)} -> t(list#128('var), list#128('value));
    instance list129 : {t('var, 'value)} -> t(list#129('var), list#129('value));
    instance list130 : {t('var, 'value)} -> t(list#130('var), list#130('value));
    instance list131 : {t('var, 'value)} -> t(list#131('var), list#131('value));
    instance list132 : {t('var, 'value)} -> t(list#132('var), list#132('value));
    instance list133 : {t('var, 'value)} -> t(list#133('var), list#133('value));
    instance list134 : {t('var, 'value)} -> t(list#134('var), list#134('value));
    instance list135 : {t('var, 'value)} -> t(list#135('var), list#135('value));
    instance list136 : {t('var, 'value)} -> t(list#136('var), list#136('value));
    instance list137 : {t('var, 'value)} -> t(list#137('var), list#137('value));
    instance list138 : {t('var, 'value)} -> t(list#138('var), list#138('value));
    instance list139 : {t('var, 'value)} -> t(list#139('var), list#139('value));
    instance list140 : {t('var, 'value)} -> t(list#140('var), list#140('value));
    instance list141 : {t('var, 'value)} -> t(list#141('var), list#141('value));
    instance list142 : {t('var, 'value)} -> t(list#142('var), list#142('value));
    instance list143 : {t('var, 'value)} -> t(list#143('var), list#143('value));
    instance list144 : {t('var, 'value)} -> t(list#144('var), list#144('value));
    instance list145 : {t('var, 'value)} -> t(list#145('var), list#145('value));
    instance list146 : {t('var, 'value)} -> t(list#146('var), list#146('value));
    instance list147 : {t('var, 'value)} -> t(list#147('var), list#147('value));
    instance list148 : {t('var, 'value)} -> t(list#148('var), list#148('value));
    instance list149 : {t('var, 'value)} -> t(list#149('var), list#149('value));
    instance list150 : {t('var, 'value)} -> t(list#150('var), list#150('value));
    instance list151 : {t('var, 'value)} -> t(list#151('var), list#151('value));
    instance list152 : {t('var, 'value)} -> t(list#152('var), list#152('value));
    instance list153 : {t('var, 'value)} -> t(list#153('var), list#153('value));
    instance list154 : {t('var, 'value)} -> t(list#154('var), list#154('value));
    instance list155 : {t('var, 'value)} -> t(list#155('var), list#155('value));
    instance list156 : {t('var, 'value)} -> t(list#156('var), list#156('value));
    instance list157 : {t('var, 'value)} -> t(list#157('var), list#157('value));
    instance list158 : {t('var, 'value)} -> t(list#158('var), list#158('value));
    instance list159 : {t('var, 'value)} -> t(list#159('var), list#159('value));
    instance list160 : {t('var, 'value)} -> t(list#160('var), list#160('value));
    instance list161 : {t('var, 'value)} -> t(list#161('var), list#161('value));
    instance list162 : {t('var, 'value)} -> t(list#162('var), list#162('value));
    instance list163 : {t('var, 'value)} -> t(list#163('var), list#163('value));
    instance list164 : {t('var, 'value)} -> t(list#164('var), list#164('value));
    instance list165 : {t('var, 'value)} -> t(list#165('var), list#165('value));
    instance list166 : {t('var, 'value)} -> t(list#166('var), list#166('value));
    instance list167 : {t('var, 'value)} -> t(list#167('var), list#167('value));
    instance list168 : {t('var, 'value)} -> t(list#168('var), list#168('value));
    instance list169 : {t('var, 'value)} -> t(list#169('var), list#169('value));
    instance list170 : {t('var, 'value)} -> t(list#170('var), list#170('value));
    instance list171 : {t('var, 'value)} -> t(list#171('var), list#171('value));
    instance list172 : {t('var, 'value)} -> t(list#172('var), list#172('value));
    instance list173 : {t('var, 'value)} -> t(list#173('var), list#173('value));
    instance list174 : {t('var, 'value)} -> t(list#174('var), list#174('value));
    instance list175 : {t('var, 'value)} -> t(list#175('var), list#175('value));
    instance list176 : {t('var, 'value)} -> t(list#176('var), list#176('value));
    instance list177 : {t('var, 'value)} -> t(list#177('var), list#177('value));
    instance list178 : {t('var, 'value)} -> t(list#178('var), list#178('value));
    instance list179 : {t('var, 'value)} -> t(list#179('var), list#179('value));
    instance list180 : {t('var, 'value)} -> t(list#180('var), list#180('value));
    instance list181 : {t('var, 'value)} -> t(list#181('var), list#181('value));
    instance list182 : {t('var, 'value)} -> t(list#182('var), list#182('value));
    instance list183 : {t('var, 'value)} -> t(list#183('var), list#183('value));
    instance list184 : {t('var, 'value)} -> t(list#184('var), list#184('value));
    instance list185 : {t('var, 'value)} -> t(list#185('var), list#185('value));
    instance list186 : {t('var, 'value)} -> t(list#186('var), list#186('value));
    instance list187 : {t('var, 'value)} -> t(list#187('var), list#187('value));
    instance list188 : {t('var, 'value)} -> t(list#188('var), list#188('value));
    instance list189 : {t('var, 'value)} -> t(list#189('var), list#189('value));
    instance list190 : {t('var, 'value)} -> t(list#190('var), list#190('value));
    instance list191 : {t('var, 'value)} -> t(list#191('var), list#191('value));
    instance list192 : {t('var, 'value)} -> t(list#192('var), list#192('value));
    instance list193 : {t('var, 'value)} -> t(list#193('var), list#193('value));
    instance list194 : {t('var, 'value)} -> t(list#194('var), list#194('value));
    instance list195 : {t('var, 'value)} -> t(list#195('var), list#195('value));
    instance list196 : {t('var, 'value)} -> t(list#196('var), list#196('value));
    instance list197 : {t('var, 'value)} -> t(list#197('var), list#197('value));
    instance list198 : {t('var, 'value)} -> t(list#198('var), list#198('value));
    instance list199 : {t('var, 'value)} -> t(list#199('var), list#199('value));
    instance list200 : {t('var, 'value)} -> t(list#200('var), list#200('value));
    instance list201 : {t('var, 'value)} -> t(list#201('var), list#201('value));
    instance list202 : {t('var, 'value)} -> t(list#202('var), list#202('value));
    instance list203 : {t('var, 'value)} -> t(list#203('var), list#203('value));
    instance list204 : {t('var, 'value)} -> t(list#204('var), list#204('value));
    instance list205 : {t('var, 'value)} -> t(list#205('var), list#205('value));
    instance list206 : {t('var, 'value)} -> t(list#206('var), list#206('value));
    instance list207 : {t('var, 'value)} -> t(list#207('var), list#207('value));
    instance list208 : {t('var, 'value)} -> t(list#208('var), list#208('value));
    instance list209 : {t('var, 'value)} -> t(list#209('var), list#209('value));
    instance list210 : {t('var, 'value)} -> t(list#210('var), list#210('value));
    instance list211 : {t('var, 'value)} -> t(list#211('var), list#211('value));
    instance list212 : {t('var, 'value)} -> t(list#212('var), list#212('value));
    instance list213 : {t('var, 'value)} -> t(list#213('var), list#213('value));
    instance list214 : {t('var, 'value)} -> t(list#214('var), list#214('value));
    instance list215 : {t('var, 'value)} -> t(list#215('var), list#215('value));
    instance list216 : {t('var, 'value)} -> t(list#216('var), list#216('value));
    instance list217 : {t('var, 'value)} -> t(list#217('var), list#217('value));
    instance list218 : {t('var, 'value)} -> t(list#218('var), list#218('value));
    instance list219 : {t('var, 'value)} -> t(list#219('var), list#219('value));
    instance list220 : {t('var, 'value)} -> t(list#220('var), list#220('value));
    instance list221 : {t('var, 'value)} -> t(list#221('var), list#221('value));
    instance list222 : {t('var, 'value)} -> t(list#222('var), list#222('value));
    instance list223 : {t('var, 'value)} -> t(list#223('var), list#223('value));
    instance list224 : {t('var, 'value)} -> t(list#224('var), list#224('value));
    instance list225 : {t('var, 'value)} -> t(list#225('var), list#225('value));
    instance list226 : {t('var, 'value)} -> t(list#226('var), list#226('value));
    instance list227 : {t('var, 'value)} -> t(list#227('var), list#227('value));
    instance list228 : {t('var, 'value)} -> t(list#228('var), list#228('value));
    instance list229 : {t('var, 'value)} -> t(list#229('var), list#229('value));
    instance list230 : {t('var, 'value)} -> t(list#230('var), list#230('value));
    instance list231 : {t('var, 'value)} -> t(list#231('var), list#231('value));
    instance list232 : {t('var, 'value)} -> t(list#232('var), list#232('value));
    instance list233 : {t('var, 'value)} -> t(list#233('var), list#233('value));
    instance list234 : {t('var, 'value)} -> t(list#234('var), list#234('value));
    instance list235 : {t('var, 'value)} -> t(list#235('var), list#235('value));
    instance list236 : {t('var, 'value)} -> t(list#236('var), list#236('value));
    instance list237 : {t('var, 'value)} -> t(list#237('var), list#237('value));
    instance list238 : {t('var, 'value)} -> t(list#238('var), list#238('value));
    instance list239 : {t('var, 'value)} -> t(list#239('var), list#239('value));
    instance list240 : {t('var, 'value)} -> t(list#240('var), list#240('value));
    instance list241 : {t('var, 'value)} -> t(list#241('var), list#241('value));
    instance list242 : {t('var, 'value)} -> t(list#242('var), list#242('value));
    instance list243 : {t('var, 'value)} -> t(list#243('var), list#243('value));
    instance list244 : {t('var, 'value)} -> t(list#244('var), list#244('value));
    instance list245 : {t('var, 'value)} -> t(list#245('var), list#245('value));
    instance list246 : {t('var, 'value)} -> t(list#246('var), list#246('value));
    instance list247 : {t('var, 'value)} -> t(list#247('var), list#247('value));
    instance list248 : {t('var, 'value)} -> t(list#248('var), list#248('value));
    instance list249 : {t('var, 'value)} -> t(list#249('var), list#249('value));
    instance list250 : {t('var, 'value)} -> t(list#250('var), list#250('value));
    instance list251 : {t('var, 'value)} -> t(list#251('var), list#251('value));
    instance list252 : {t('var, 'value)} -> t(list#252('var), list#252('value));
    instance list253 : {t('var, 'value)} -> t(list#253('var), list#253('value));
    instance list254 : {t('var, 'value)} -> t(list#254('var), list#254('value));
    instance list255 : {t('var, 'value)} -> t(list#255('var), list#255('value));
    instance list256 : {t('var, 'value)} -> t(list#256('var), list#256('value));
  };

  module Boolean : {
    type var = bool_var;

    type value = bool;

    let to_field : var -> field_var;

    let true_ : var;

    let false_ : var;

    let if_ : var -> then_:var -> else_:var -> var;

    let not : var -> var;

    let (&&) : var -> var -> var;

    let (||) : var -> var -> var;

    let lxor : var -> var -> var;

    let any : list(var) -> var;

    let all : list(var) -> var;

    let of_field : field_var -> var;

    let var_of_value : value -> var;

    instance typ : Typ.t(bool_var, bool);

    instance typ : Typ.t(t, bool);

    let equal : var -> var -> var;

    module Unsafe : { let of_cvar : field_var -> var; };

    module Assert : {
      let (=) : var -> var -> unit;

      let equal : var -> var -> unit;

      let is_true : var -> unit;

      let any : list(var) -> unit;

      let all : list(var) -> unit;

      let exactly_one : list(var) -> unit;

    };

  };

  module Field : {
    module Constant : {
      type t = field;

      let compare : t -> t -> int;

      let of_int : int -> t;

      let one : t;

      let zero : t;

      let add : t -> t -> t;

      let sub : t -> t -> t;

      let mul : t -> t -> t;

      let inv : t -> t;

      let square : t -> t;

      let sqrt : t -> t;

      let is_square : t -> bool;

      let equal : t -> t -> bool;

      let size_in_bits : int;

      let print : t -> unit;

      let random : unit -> t;

      let negate : t -> t;

      let (+) : t -> t -> t;

      let ( * ) : t -> t -> t;

      let (-) : t -> t -> t;

      let (/) : t -> t -> t;

      let of_string : string -> t;

      let to_string : t -> string;

      let unpack : t -> list(bool);
      let to_bits : t -> list(bool);

      let project : list(bool) -> t;
      let of_bits : list(bool) -> t;
    };

    type t = field_var;

    let size_in_bits : int;

    let length : t -> int;

    let constant : field -> t;

    let of_string : string -> t;

    let to_constant : t -> option(field);

    let linear_combination : list((field, t)) -> t;

    let sum : list(t) -> t;

    let add : t -> t -> t;

    let sub : t -> t -> t;

    let scale : t -> field -> t;

    let project : list(Boolean.var) -> t;

    let pack : list(Boolean.var) -> t;

    let of_int : int -> t;

    let one : t;

    let zero : t;

    let mul : t -> t -> t;

    let square : t -> t;

    let div : t -> t -> t;

    let inv : t -> t;

    let equal : t -> t -> Boolean.var;

    let unpack : t -> length:int -> list(Boolean.var);

    let unpack_full : t -> list(Boolean.var);

    let choose_preimage_var : t -> length:int -> list(Boolean.var);

    let to_bits : ?length:int -> t -> list(Boolean.var);

    let of_bits : list(Boolean.var) -> t;

    type comparison_result =
      {less: Boolean.var, less_or_equal: Boolean.var};

    let compare : bit_length:int -> t -> t -> comparison_result;

    let if_ : Boolean.var -> then_:t -> else_:t -> t;

    let (+) : t -> t -> t;

    let (-) : t -> t -> t;

    let ( * ) : t -> t -> t;

    let (/) : t -> t -> t;

    module Unsafe : { let of_index : int -> t;  };

    module Assert : {
      let lte : bit_length:int -> t -> t -> unit;

      let gte : bit_length:int -> t -> t -> unit;

      let lt : bit_length:int -> t -> t -> unit;

      let gt : bit_length:int -> t -> t -> unit;

      let not_equal : t -> t -> unit;

      let equal : t -> t -> unit;

      let non_zero : t -> unit;

    };

    /* Nuclear option: instances for all the different combinations. */
    instance typ : Typ.t(t, Constant.t);

    instance typ : Typ.t(t, field);

    instance typ : Typ.t(t, field);

    instance typ : Typ.t(field_var, field);

  };

  let load_pedersen_params : string -> array(((field_var, field_var), (field_var, field_var), (field_var, field_var),  (field_var, field_var)));

  module Select : {
    type t('a) = Boolean.var -> then_:'a -> else_:'a -> 'a;

    let id : {t('a)} -> t('a);

    instance field : t(Field.t);
    instance boolean : t(Boolean.var);
    instance tuple2: {t('a1)} -> {t('a2)} -> t(('a1, 'a2));

    instance list: {t('a)} -> t(list('a));
    instance array: {t('a)} -> t(array('a));
  };

  module Bitstring_checked : {
    type t = list(Boolean.var);

    let equal : t -> t -> Boolean.var;

    let lt_value : list(Boolean.var) -> list(bool) -> Boolean.var;

    module Assert : { let equal : t -> t -> unit;  };

  };

  module As_prover : {
    type t('a) = 'a;

    let in_prover_block : unit -> bool;

    let read_var : Field.t -> Field.Constant.t;

    let read : {Typ.t('var, 'value)} -> 'var -> 'value;

    let of_int : int -> field;

    let one : field;

    let zero : field;

    let add : field -> field -> field;

    let sub : field -> field -> field;

    let mul : field -> field -> field;

    let inv : field -> field;

    let square : field -> field;

    let sqrt : field -> field;

    let is_square : field -> bool;

    let equal : field -> field -> bool;

    let size_in_bits : int;

    let print : field -> unit;

    let random : unit -> field;

    let to_string : field -> string;

    let negate : field -> field;

    let (+) : field -> field -> field;

    let ( * ) : field -> field -> field;

    let (-) : field -> field -> field;

    let (/) : field -> field -> field;

    let unpack : field -> list(bool);

    let project : list(bool) -> field;

  };

  module Handle : {
    type t('var, 'value);

    let value : t(_, 'value) -> As_prover.t(unit -> 'value);

    let var : t('var, _) -> 'var;

  };

  type response = Request.response;

  let unhandled : response;

  type request =
    | With
        { request: Request.t('a)
        , respond: Request.Response.t('a) -> response}
      : request;

  module Handler : {
    type t = request -> response;
  };

  let assert_ : Constraint.t -> unit;

  let assert_all : list(Constraint.t) -> unit;

  let assert_r1cs :
    Field.t -> Field.t -> Field.t -> unit;

  let assert_square : Field.t -> Field.t -> unit;

  let as_prover : As_prover.t(unit -> unit) -> unit;

  let next_auxiliary : unit -> int;

  let request_witness :
       {Typ.t('var, 'value)}
    -> As_prover.t(unit -> Request.t('value))
    -> 'var;

  let perform : As_prover.t(unit -> Request.t(unit)) -> unit;

  let request :
       ?such_that:('var -> unit)
    -> {Typ.t('var, 'value)}
    -> Request.t('value)
    -> 'var;

  let exists :
       {Typ.t('var, 'value)}
    -> ?request:As_prover.t(unit -> Request.t('value))
    -> ?compute:As_prover.t(unit -> 'value)
    -> 'var;

  let exists_handle :
       {Typ.t('var, 'value)}
    -> ?request:As_prover.t(unit -> Request.t('value))
    -> ?compute:As_prover.t(unit -> 'value)
    -> Handle.t('var, 'value);

  let handle : (unit -> 'a) -> Handler.t -> 'a;

  let handle_as_prover :
    (unit -> 'a) -> As_prover.t((unit -> Handler.t)) -> 'a;

  let with_label : string -> (unit -> 'a) -> 'a;

  let make_checked : (unit -> 'a) -> Typ.checked_secret('a);

  module Number : {
    type t;

    let (+) : t -> t -> t;

    let (-) : t -> t -> t;

    let ( * ) : t -> t -> t;

    let constant : field -> t;

    let one : t;

    let zero : t;

    let if_ : Boolean.var -> then_:t -> else_:t -> t;

    let (<) : t -> t -> Boolean.var;

    let (>) : t -> t -> Boolean.var;

    let (<=) : t -> t -> Boolean.var;

    let (>=) : t -> t -> Boolean.var;

    let (=) : t -> t -> Boolean.var;

    let min : t -> t -> t;

    let max : t -> t -> t;

    let to_var : t -> Field.t;

    let of_bits : list(Boolean.var) -> t;

    let to_bits : t -> list(Boolean.var);

    let clamp_to_n_bits : t -> int -> t;

  };
|}
  )

let checked =
  ( __LINE__ + 1
  , {|
  let read_lines = read_lines;
  let load_pedersen_params = load_pedersen_params;

  module Constraint = {
    type t = Constraint.t;

    let boolean = Constraint.boolean;

    let equal = Constraint.equal;

    let r1cs = Constraint.r1cs;

    let square = Constraint.square;
  };

  module Boolean = {
    type t = Boolean.var;

    let to_field = Boolean.to_field;

    let true_ = Boolean.true_;

    let false_ = Boolean.false_;

    let if_ = Boolean.if_;

    let not = Boolean.not;

    let (&&) = Boolean.(&&);

    let (||) = Boolean.(||);

    let lxor = Boolean.lxor;

    let any = Boolean.any;

    let all = Boolean.all;

    let equal = Boolean.equal;

    module Unsafe = {
      let of_field = Boolean.Unsafe.of_cvar;
    };

    module Assert = {
      let (=) = Boolean.Assert.(=);
      let equal = Boolean.Assert.equal;

      let is_true = Boolean.Assert.is_true;

      let any = Boolean.Assert.any;

      let all = Boolean.Assert.all;

      let exactly_one = Boolean.Assert.exactly_one;
    };
  };

  module Field = {
    type t = Field.t;

    module Constant = {
      type t = Field.Constant.t;

      let compare = Field.Constant.compare ;

      let of_int = Field.Constant.of_int ;

      let one = Field.Constant.one ;

      let zero = Field.Constant.zero ;

      let add = Field.Constant.add ;

      let sub = Field.Constant.sub ;

      let mul = Field.Constant.mul ;

      let inv = Field.Constant.inv ;

      let square = Field.Constant.square ;

      let sqrt = Field.Constant.sqrt ;

      let is_square = Field.Constant.is_square ;

      let equal = Field.Constant.equal ;

      let size_in_bits = Field.Constant.size_in_bits ;

      let print = Field.Constant.print ;

      let random = Field.Constant.random ;

      let negate = Field.Constant.negate ;

      let (+) = Field.Constant.(+) ;

      let ( * ) = Field.Constant.( * ) ;

      let (-) = Field.Constant.(-) ;

      let (/) = Field.Constant.(/) ;

      let of_string = Field.Constant.of_string ;

      let to_string = Field.Constant.to_string ;

      let unpack = Field.Constant.unpack ;
      let to_bits = Field.Constant.to_bits ;

      let project = Field.Constant.project ;
      let of_bits = Field.Constant.of_bits ;
    };

    let size_in_bits = Field.size_in_bits;

    let length = Field.length;

    let constant = Field.constant;

    let of_string = Field.of_string;

    let to_constant = Field.to_constant;

    let linear_combination = Field.linear_combination;

    let sum = Field.sum;

    let add = Field.add;

    let sub = Field.sub;

    let scale = Field.scale;

    let project = Field.project;

    let pack = Field.pack;

    let of_int = Field.of_int;

    let one = Field.one;

    let zero = Field.zero;

    let mul = Field.mul;

    let square = Field.square;

    let div = Field.div;

    let inv = Field.inv;

    let equal = Field.equal;

    let unpack = Field.unpack;

    let unpack_full = Field.unpack_full;

    let choose_preimage_var = Field.choose_preimage_var;

    let to_bits = Field.to_bits;

    let of_bits = Field.of_bits;

    type comparison_result = Field.comparison_result;

    let compare = Field.compare;

    let if_ = Field.if_;

    let (+) = Field.(+);

    let (-) = Field.(-);

    let ( * ) = Field.( * );

    let (/) = Field.(/);

    module Assert = {
      let lte = Field.Assert.lte;

      let gte = Field.Assert.gte;

      let lt = Field.Assert.lt;

      let gt = Field.Assert.gt;

      let not_equal = Field.Assert.not_equal;

      let equal = Field.Assert.equal;

      let non_zero = Field.Assert.non_zero;

    };
  };

  let select = Select.id;

  module Bitstring = {
    type t = Bitstring_checked.t;

    let equal = Bitstring_checked.equal;

    let lt_value = Bitstring_checked.lt_value;

    module Assert = {
      let equal = Bitstring_checked.Assert.equal;
    };

  };

  module Handle = {
    type t = Handle.t;

    let var = Handle.var;

  };

  module Handler = {
    type t = Handler.t;
  };

  let assert_ = assert_;

  let assert_all = assert_all;

  let assert_r1cs = assert_r1cs;

  let assert_r1 = assert_r1cs;

  let assert_square = assert_square;

  let as_prover = as_prover;

  let next_auxiliary = next_auxiliary;

  let request_witness = request_witness;

  let perform = perform;

  let request = request;

  let exists = exists;

  let exists_handle = exists_handle;

  let handle = handle;

  let handle_as_prover = handle_as_prover;

  let with_label = with_label;

  let make_checked = make_checked;

  module Number = {
    type t = Number.t;

    let (+) = Number.(+);

    let (-) = Number.(-);

    let ( * ) = Number.( * );

    let constant = Number.constant;

    let one = Number.one;

    let zero = Number.zero;

    let if_ = Number.if_;

    let (<) = Number.(<);

    let (>) = Number.(>);

    let (<=) = Number.(<=);

    let (>=) = Number.(>=);

    let (=) = Number.(=);

    let min = Number.min;

    let max = Number.max;

    let to_var = Number.to_var;

    let of_bits = Number.of_bits;

    let to_bits = Number.to_bits;

    let clamp_to_n_bits = Number.clamp_to_n_bits;

  };
|}
  )

let prover =
  ( __LINE__ + 1
  , {|
  let read_lines = read_lines;

  module Request = {
    type t = Request.t;

    type req = Request.t;

    module Response = {
      type t = Request.Response.t;
    };

    type response = Request.response;

    let unhandled = unhandled;

    /* Bring constructors into scope without bringing the name itself. */
    type _ += Request.Response.Provide;
  };

  module Typ = {
    module Store = {
      type t = Typ.Store.t;

      let bind = Typ.Store.bind;

      let return = Typ.Store.return;

      let map = Typ.Store.map;

      let store = Typ.Store.store;
    };

    module Alloc = {
      type t = Typ.Alloc.t;

      let bind = Typ.Alloc.bind;

      let return = Typ.Alloc.return;

      let map = Typ.Alloc.map;

      let alloc = Typ.Alloc.alloc;
    };

    module Read = {
      type t = Typ.Read.t;

      let bind = Typ.Read.bind;

      let return = Typ.Read.return;

      let map = Typ.Read.map;

      let read = Typ.Read.read;
    };


    type t = Typ.t;

    let store = Typ.store;

    let read = Typ.read;

    let alloc = Typ.alloc;

    instance check = Typ.check;

    instance unit = Typ.unit;

    let field = Typ.field;

    let tuple2 = Typ.tuple2;

    instance ( * ) = Typ.( * );

    instance tuple3 = Typ.tuple3;

    let list = Typ.list;
  };

  module Field = {
    type t = Field.Constant.t;

    type variable = Field.t;

    let compare = Field.Constant.compare;

    let of_int = Field.Constant.of_int;

    let one = Field.Constant.one;

    let zero = Field.Constant.zero;

    let add = Field.Constant.add;

    let sub = Field.Constant.sub;

    let mul = Field.Constant.mul;

    let inv = Field.Constant.inv;

    let square = Field.Constant.square;

    let sqrt = Field.Constant.sqrt;

    let is_square = Field.Constant.is_square;

    let equal = Field.Constant.equal;

    let size_in_bits = Field.Constant.size_in_bits;

    let print = Field.Constant.print;

    let random = Field.Constant.random;

    let negate = Field.Constant.negate;

    let (+) = Field.Constant.(+);

    let ( * ) = Field.Constant.( * );

    let (-) = Field.Constant.(-);

    let (/) = Field.Constant.(/);

    let of_string = Field.Constant.of_string;

    let to_string = Field.Constant.to_string;

    let unpack = Field.Constant.unpack;

    let to_bits = Field.Constant.to_bits;

    let project = Field.Constant.project;

    let of_bits = Field.Constant.of_bits;

    let typ = Field.typ;

  };

  module As_prover = {
    type t = As_prover.t;

    let in_prover_block = As_prover.in_prover_block;

    let read_var = As_prover.read_var;

    let read = As_prover.read;

    let of_int = As_prover.of_int;

    let one = As_prover.one;

    let zero = As_prover.zero;

    let add = As_prover.add;

    let sub = As_prover.sub;

    let mul = As_prover.mul;

    let inv = As_prover.inv;

    let square = As_prover.square;

    let sqrt = As_prover.sqrt;

    let is_square = As_prover.is_square;

    let equal = As_prover.equal;

    let size_in_bits = As_prover.size_in_bits;

    let print = As_prover.print;

    let random = As_prover.random;

    let to_string = As_prover.to_string;

    let negate = As_prover.negate;

    let (+) = As_prover.(+);

    let ( * ) = As_prover.( * );

    let (-) = As_prover.(-);

    let (/) = As_prover.(/);

    let unpack = As_prover.unpack;

    let project = As_prover.project;
  };

  module Handle = {
    type t = Handle.t;

    let value = Handle.value;
  };

  type response = response;

  let unhandled = unhandled;

  type request = request;

  /* Bring constructors into scope without bringing the name itself. */
  type _ += Request.Response.Provide;

  module Handler = {
    type t = Handler.t;
  };

  let in_prover_block = As_prover.in_prover_block;

  let read_var = As_prover.read_var;

  let read = As_prover.read;

  let of_int = As_prover.of_int;

  let one = As_prover.one;

  let zero = As_prover.zero;

  let add = As_prover.add;

  let sub = As_prover.sub;

  let mul = As_prover.mul;

  let inv = As_prover.inv;

  let square = As_prover.square;

  let sqrt = As_prover.sqrt;

  let is_square = As_prover.is_square;

  let equal = As_prover.equal;

  let size_in_bits = As_prover.size_in_bits;

  let print = As_prover.print;

  let random = As_prover.random;

  let to_string = As_prover.to_string;

  let negate = As_prover.negate;

  let (+) = As_prover.(+);

  let ( * ) = As_prover.( * );

  let (-) = As_prover.(-);

  let (/) = As_prover.(/);

  let unpack = As_prover.unpack;

  let project = As_prover.project;
|}
  )
