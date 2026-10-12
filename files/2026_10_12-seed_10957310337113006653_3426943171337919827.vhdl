-- Seed: 10957310337113006653,3426943171337919827

entity spbo is
  port (iwyqlogd : linkage integer; ocutoy : in real; kaionwei : linkage time; kptkwj : out real);
end spbo;

architecture ogomsyt of spbo is
  
begin
  -- Single-driven assignments
  kptkwj <= 0.4_1_3_4;
end ogomsyt;

entity twefqsxopc is
  port (mlxtrtutn : linkage integer; yna : inout time);
end twefqsxopc;

architecture lxtapns of twefqsxopc is
  signal lh : time;
  signal ty : real;
begin
  pgvldlksb : entity work.spbo
    port map (iwyqlogd => mlxtrtutn, ocutoy => ty, kaionwei => lh, kptkwj => ty);
  
  -- Single-driven assignments
  yna <= 0 ps;
end lxtapns;

entity dgpdkyoisi is
  port (oxocfne : linkage time; chfxvntv : out real; ofa : buffer time);
end dgpdkyoisi;

architecture ywxgsm of dgpdkyoisi is
  signal zy : integer;
  signal cdvecjyjnr : real;
  signal ntximihm : time;
  signal fafknu : real;
  signal ns : integer;
  signal v : time;
  signal dyqpar : integer;
begin
  uakbt : entity work.twefqsxopc
    port map (mlxtrtutn => dyqpar, yna => v);
  armutmxs : entity work.spbo
    port map (iwyqlogd => ns, ocutoy => fafknu, kaionwei => ntximihm, kptkwj => cdvecjyjnr);
  veisszkoe : entity work.twefqsxopc
    port map (mlxtrtutn => zy, yna => ofa);
  
  -- Single-driven assignments
  fafknu <= chfxvntv;
  chfxvntv <= 16#F.A_C_C_6_6#;
end ywxgsm;

entity gn is
  port (wh : out bit_vector(3 downto 2); fktlvc : in real);
end gn;

architecture pgrqcoj of gn is
  signal kd : time;
  signal awzfbw : real;
  signal fvotuvyjhr : time;
begin
  zskydqsca : entity work.dgpdkyoisi
    port map (oxocfne => fvotuvyjhr, chfxvntv => awzfbw, ofa => kd);
  
  -- Single-driven assignments
  wh <= ('1', '0');
end pgrqcoj;



-- Seed after: 12780516043046450622,3426943171337919827
