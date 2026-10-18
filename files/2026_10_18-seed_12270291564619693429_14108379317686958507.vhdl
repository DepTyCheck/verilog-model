-- Seed: 12270291564619693429,14108379317686958507

library ieee;
use ieee.std_logic_1164.all;

entity fanzietv is
  port (qjhlg : in time_vector(4 to 4); sqcrhay : out real; vcmbpadbxz : in std_logic);
end fanzietv;

architecture yrspjgffwd of fanzietv is
  
begin
  -- Single-driven assignments
  sqcrhay <= 8#21334.5_6_4_5_1#;
end yrspjgffwd;

entity yzudnlkzv is
  port (ydp : in integer; munpo : buffer real; gymxanxjzw : linkage boolean);
end yzudnlkzv;

library ieee;
use ieee.std_logic_1164.all;

architecture kdggze of yzudnlkzv is
  signal jzpcf : time_vector(4 to 4);
  signal yggtuoma : std_logic;
  signal xjsh : real;
  signal gzdxlnh : time_vector(4 to 4);
begin
  gtwukkudsi : entity work.fanzietv
    port map (qjhlg => gzdxlnh, sqcrhay => xjsh, vcmbpadbxz => yggtuoma);
  uvkbe : entity work.fanzietv
    port map (qjhlg => jzpcf, sqcrhay => munpo, vcmbpadbxz => yggtuoma);
  
  -- Single-driven assignments
  gzdxlnh <= (others => 41004 fs);
  jzpcf <= (others => 16#801A8.7_6_0_8# ps);
  
  -- Multi-driven assignments
  yggtuoma <= 'X';
  yggtuoma <= '0';
  yggtuoma <= '1';
end kdggze;

entity l is
  port (s : in integer; swvlktxhut : inout bit_vector(0 downto 4); veo : out time);
end l;

library ieee;
use ieee.std_logic_1164.all;

architecture thd of l is
  signal gaaotroc : boolean;
  signal q : real;
  signal d : integer;
  signal fmkikv : std_logic;
  signal uap : real;
  signal m : time_vector(4 to 4);
  signal abrqvrccd : std_logic;
  signal zm : real;
  signal nju : std_logic;
  signal xrumqezi : real;
  signal hhebtbmm : time_vector(4 to 4);
begin
  yhzrwvgpw : entity work.fanzietv
    port map (qjhlg => hhebtbmm, sqcrhay => xrumqezi, vcmbpadbxz => nju);
  rpvgvoswzw : entity work.fanzietv
    port map (qjhlg => hhebtbmm, sqcrhay => zm, vcmbpadbxz => abrqvrccd);
  whaisujgce : entity work.fanzietv
    port map (qjhlg => m, sqcrhay => uap, vcmbpadbxz => fmkikv);
  vmsyoniga : entity work.yzudnlkzv
    port map (ydp => d, munpo => q, gymxanxjzw => gaaotroc);
  
  -- Multi-driven assignments
  nju <= nju;
  nju <= nju;
  fmkikv <= nju;
end thd;

library ieee;
use ieee.std_logic_1164.all;

entity zkcp is
  port (fieckptok : in std_logic; wadptmqyzf : in std_logic; zdznec : out real; li : inout std_logic);
end zkcp;

library ieee;
use ieee.std_logic_1164.all;

architecture pszozlravg of zkcp is
  signal xgkygw : time_vector(4 to 4);
  signal rtkdbto : std_logic;
  signal k : real;
  signal wvupnadh : time_vector(4 to 4);
  signal foqgjyssn : real;
  signal osigwitfad : time_vector(4 to 4);
begin
  i : entity work.fanzietv
    port map (qjhlg => osigwitfad, sqcrhay => foqgjyssn, vcmbpadbxz => wadptmqyzf);
  yjxmcv : entity work.fanzietv
    port map (qjhlg => wvupnadh, sqcrhay => k, vcmbpadbxz => rtkdbto);
  fyt : entity work.fanzietv
    port map (qjhlg => xgkygw, sqcrhay => zdznec, vcmbpadbxz => li);
  
  -- Multi-driven assignments
  li <= 'X';
  li <= '1';
  li <= wadptmqyzf;
end pszozlravg;



-- Seed after: 1260696161238474887,14108379317686958507
