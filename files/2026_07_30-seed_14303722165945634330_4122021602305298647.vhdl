-- Seed: 14303722165945634330,4122021602305298647

library ieee;
use ieee.std_logic_1164.all;

entity nm is
  port (bap : linkage std_logic; wzlfzzd : out real);
end nm;

architecture xozan of nm is
  
begin
  
end xozan;

library ieee;
use ieee.std_logic_1164.all;

entity rzjbfl is
  port (cpeo : inout std_logic);
end rzjbfl;

library ieee;
use ieee.std_logic_1164.all;

architecture tvgs of rzjbfl is
  signal hpurqf : real;
  signal vzbtefatz : std_logic;
  signal ucfwrgr : real;
  signal jqjb : real;
begin
  llch : entity work.nm
    port map (bap => cpeo, wzlfzzd => jqjb);
  kwttypt : entity work.nm
    port map (bap => cpeo, wzlfzzd => ucfwrgr);
  upnlktflgd : entity work.nm
    port map (bap => vzbtefatz, wzlfzzd => hpurqf);
  
  -- Multi-driven assignments
  cpeo <= cpeo;
  cpeo <= cpeo;
end tvgs;

entity zgvvektea is
  port (zqyguij : in integer; mde : out integer; jkjfgdk : in time; xcsqizy : linkage integer_vector(0 to 1));
end zgvvektea;

architecture rykmanl of zgvvektea is
  
begin
  -- Single-driven assignments
  mde <= mde;
end rykmanl;

library ieee;
use ieee.std_logic_1164.all;

entity u is
  port (txchmmx : out std_logic_vector(4 to 4); gpuflumn : linkage integer; ox : buffer integer);
end u;

library ieee;
use ieee.std_logic_1164.all;

architecture wkx of u is
  signal zbl : integer_vector(0 to 1);
  signal toog : time;
  signal kouotnroc : real;
  signal izz : std_logic;
  signal nlwwhg : std_logic;
  signal lvzemvp : real;
  signal bhgqtbloq : std_logic;
begin
  ztculqmfh : entity work.nm
    port map (bap => bhgqtbloq, wzlfzzd => lvzemvp);
  vterlla : entity work.rzjbfl
    port map (cpeo => nlwwhg);
  kumzsty : entity work.nm
    port map (bap => izz, wzlfzzd => kouotnroc);
  qbzrxyzuf : entity work.zgvvektea
    port map (zqyguij => ox, mde => ox, jkjfgdk => toog, xcsqizy => zbl);
  
  -- Single-driven assignments
  toog <= toog;
  
  -- Multi-driven assignments
  txchmmx <= (others => 'Z');
  txchmmx <= "W";
  nlwwhg <= '-';
  txchmmx <= (others => '1');
end wkx;



-- Seed after: 8191237769514510585,4122021602305298647
