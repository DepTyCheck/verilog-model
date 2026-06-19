-- Seed: 1586875634634117647,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity kcgp is
  port (qnafemqym : inout boolean_vector(3 to 4); vcpfc : inout integer; z : linkage real; b : in std_logic);
end kcgp;

architecture cufeywzwa of kcgp is
  
begin
  -- Single-driven assignments
  vcpfc <= 8#0#;
  qnafemqym <= (FALSE, TRUE);
end cufeywzwa;

library ieee;
use ieee.std_logic_1164.all;

entity tuea is
  port (mka : in std_logic_vector(1 downto 0));
end tuea;

library ieee;
use ieee.std_logic_1164.all;

architecture acanxum of tuea is
  signal u : std_logic;
  signal wsx : real;
  signal hyq : integer;
  signal phwaplmsu : boolean_vector(3 to 4);
  signal wwjdbnowqh : std_logic;
  signal oshsss : real;
  signal jtfqujzd : integer;
  signal skfr : boolean_vector(3 to 4);
begin
  tt : entity work.kcgp
    port map (qnafemqym => skfr, vcpfc => jtfqujzd, z => oshsss, b => wwjdbnowqh);
  h : entity work.kcgp
    port map (qnafemqym => phwaplmsu, vcpfc => hyq, z => wsx, b => u);
  
  -- Multi-driven assignments
  wwjdbnowqh <= 'U';
end acanxum;

entity ay is
  port (eqvlluj : inout real; isjirql : buffer time_vector(3 to 1); at : inout real);
end ay;

library ieee;
use ieee.std_logic_1164.all;

architecture r of ay is
  signal hspbpwb : std_logic_vector(1 downto 0);
begin
  no : entity work.tuea
    port map (mka => hspbpwb);
  yfrfhs : entity work.tuea
    port map (mka => hspbpwb);
  
  -- Single-driven assignments
  at <= 2#1.0_0_1_0_0#;
  isjirql <= (others => 0 ns);
  
  -- Multi-driven assignments
  hspbpwb <= ('1', 'W');
end r;

entity nneoxkmyys is
  port (caes : out integer; qvdeilkq : inout real; wnmxfjaq : in time);
end nneoxkmyys;

library ieee;
use ieee.std_logic_1164.all;

architecture ibllqoog of nneoxkmyys is
  signal gbastxrbj : std_logic;
  signal h : integer;
  signal cqegsy : boolean_vector(3 to 4);
  signal tums : std_logic;
  signal zjkyp : real;
  signal qzmfhrvtqz : integer;
  signal tkt : boolean_vector(3 to 4);
  signal pwsgeogtb : real;
  signal vu : time_vector(3 to 1);
  signal quuigoq : real;
  signal dfwops : std_logic;
  signal dqfr : real;
  signal knbfobdvek : boolean_vector(3 to 4);
begin
  s : entity work.kcgp
    port map (qnafemqym => knbfobdvek, vcpfc => caes, z => dqfr, b => dfwops);
  drq : entity work.ay
    port map (eqvlluj => quuigoq, isjirql => vu, at => pwsgeogtb);
  kidakqs : entity work.kcgp
    port map (qnafemqym => tkt, vcpfc => qzmfhrvtqz, z => zjkyp, b => tums);
  eas : entity work.kcgp
    port map (qnafemqym => cqegsy, vcpfc => h, z => qvdeilkq, b => gbastxrbj);
  
  -- Multi-driven assignments
  dfwops <= 'W';
  dfwops <= 'H';
  dfwops <= 'Z';
  dfwops <= 'L';
end ibllqoog;



-- Seed after: 8864383063990003443,3108530264173481209
