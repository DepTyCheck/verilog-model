-- Seed: 6235818259728009563,662889661651915549

entity mif is
  port (iman : linkage string(4 to 1); lgk : in real);
end mif;

architecture dwsjg of mif is
  
begin
  
end dwsjg;

entity pnxllib is
  port (qupofk : out time; i : linkage boolean; mpyynkvmst : in real; veapoyhs : linkage integer);
end pnxllib;

architecture fus of pnxllib is
  signal tg : string(4 to 1);
  signal xucumkeqk : string(4 to 1);
  signal iebibgz : string(4 to 1);
  signal yobja : real;
  signal uwtqqkzhb : string(4 to 1);
begin
  ca : entity work.mif
    port map (iman => uwtqqkzhb, lgk => yobja);
  hvugyec : entity work.mif
    port map (iman => iebibgz, lgk => mpyynkvmst);
  ch : entity work.mif
    port map (iman => xucumkeqk, lgk => mpyynkvmst);
  yhmgcs : entity work.mif
    port map (iman => tg, lgk => mpyynkvmst);
  
  -- Single-driven assignments
  qupofk <= qupofk;
  yobja <= 2#111.0_0_1_1_0#;
end fus;

entity zzcqljijs is
  port (hxq : buffer time; huawuqhok : buffer real; eyxmb : inout integer; rrgh : out real);
end zzcqljijs;

architecture lvub of zzcqljijs is
  signal zwt : real;
  signal o : string(4 to 1);
  signal prg : string(4 to 1);
  signal zejdq : string(4 to 1);
  signal azxj : real;
  signal fp : boolean;
  signal r : time;
begin
  tshpx : entity work.pnxllib
    port map (qupofk => r, i => fp, mpyynkvmst => azxj, veapoyhs => eyxmb);
  wtkgoxl : entity work.mif
    port map (iman => zejdq, lgk => rrgh);
  xdqvgupst : entity work.mif
    port map (iman => prg, lgk => rrgh);
  mvosefyzsm : entity work.mif
    port map (iman => o, lgk => zwt);
  
  -- Single-driven assignments
  rrgh <= 3.0;
  hxq <= hxq;
  huawuqhok <= huawuqhok;
end lvub;

entity egdl is
  port (mgcyxxmt : in integer; k : inout integer);
end egdl;

architecture epmynqm of egdl is
  
begin
  -- Single-driven assignments
  k <= 2#10#;
end epmynqm;



-- Seed after: 1032764627857867704,662889661651915549
