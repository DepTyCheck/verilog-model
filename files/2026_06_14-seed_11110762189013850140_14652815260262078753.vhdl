-- Seed: 11110762189013850140,14652815260262078753

entity vaqrnoz is
  port (xtpp : inout character; ntjkmnt : out time);
end vaqrnoz;

architecture dvjvlit of vaqrnoz is
  
begin
  -- Single-driven assignments
  ntjkmnt <= 2_2 fs;
  xtpp <= 'o';
end dvjvlit;

library ieee;
use ieee.std_logic_1164.all;

entity rzfvevbdkr is
  port (vhfdygzz : buffer std_logic_vector(3 to 4));
end rzfvevbdkr;

architecture sptg of rzfvevbdkr is
  signal nqm : time;
  signal so : character;
  signal p : time;
  signal ivfudvnu : character;
begin
  wrckzxl : entity work.vaqrnoz
    port map (xtpp => ivfudvnu, ntjkmnt => p);
  knk : entity work.vaqrnoz
    port map (xtpp => so, ntjkmnt => nqm);
end sptg;

library ieee;
use ieee.std_logic_1164.all;

entity ljutfrpb is
  port (cybapq : linkage integer_vector(2 to 0); tie : in time; vrqpclbgj : in std_logic; wersivupih : linkage std_logic_vector(3 to 2));
end ljutfrpb;

architecture noirjho of ljutfrpb is
  signal ik : time;
  signal yjkd : character;
  signal tdsrwtrkp : time;
  signal cyp : character;
begin
  r : entity work.vaqrnoz
    port map (xtpp => cyp, ntjkmnt => tdsrwtrkp);
  bjt : entity work.vaqrnoz
    port map (xtpp => yjkd, ntjkmnt => ik);
end noirjho;

library ieee;
use ieee.std_logic_1164.all;

entity ypwsje is
  port (xwbgauqtr : out std_logic_vector(2 to 1); fudhhczm : inout time_vector(2 downto 1); sjmlplden : linkage time);
end ypwsje;

library ieee;
use ieee.std_logic_1164.all;

architecture xsbcfmfz of ypwsje is
  signal tt : time;
  signal euqu : character;
  signal bzktngtk : std_logic_vector(3 to 4);
begin
  ioiv : entity work.rzfvevbdkr
    port map (vhfdygzz => bzktngtk);
  skpihjrggc : entity work.vaqrnoz
    port map (xtpp => euqu, ntjkmnt => tt);
  
  -- Single-driven assignments
  fudhhczm <= (2#11# ms, 2#011# ps);
  
  -- Multi-driven assignments
  bzktngtk <= "HH";
  xwbgauqtr <= (others => '0');
end xsbcfmfz;



-- Seed after: 15316987893424728540,14652815260262078753
