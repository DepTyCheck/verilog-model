-- Seed: 2535787323424351829,3316342841050048249

library ieee;
use ieee.std_logic_1164.all;

entity uqtyfhe is
  port (ac : inout real; vnbby : in std_logic; ebjg : out std_logic_vector(0 downto 1));
end uqtyfhe;

architecture mxk of uqtyfhe is
  
begin
  
end mxk;

entity ersofm is
  port (riswxjry : in time; amyl : inout integer);
end ersofm;

library ieee;
use ieee.std_logic_1164.all;

architecture gq of ersofm is
  signal q : real;
  signal lqm : std_logic_vector(0 downto 1);
  signal rgohq : std_logic;
  signal dvpg : real;
begin
  rdzlolg : entity work.uqtyfhe
    port map (ac => dvpg, vnbby => rgohq, ebjg => lqm);
  ugclev : entity work.uqtyfhe
    port map (ac => q, vnbby => rgohq, ebjg => lqm);
  
  -- Single-driven assignments
  amyl <= amyl;
  
  -- Multi-driven assignments
  lqm <= (others => '0');
end gq;

library ieee;
use ieee.std_logic_1164.all;

entity cqqje is
  port (te : buffer std_logic_vector(4 to 3));
end cqqje;

library ieee;
use ieee.std_logic_1164.all;

architecture p of cqqje is
  signal rxf : std_logic_vector(0 downto 1);
  signal a : real;
  signal nacsg : real;
  signal gzkehcv : std_logic_vector(0 downto 1);
  signal ygivtsbao : std_logic;
  signal mhabuszmgy : real;
begin
  pxbfb : entity work.uqtyfhe
    port map (ac => mhabuszmgy, vnbby => ygivtsbao, ebjg => gzkehcv);
  pgdr : entity work.uqtyfhe
    port map (ac => nacsg, vnbby => ygivtsbao, ebjg => te);
  vdg : entity work.uqtyfhe
    port map (ac => a, vnbby => ygivtsbao, ebjg => rxf);
  
  -- Multi-driven assignments
  te <= te;
end p;

library ieee;
use ieee.std_logic_1164.all;

entity um is
  port (tzbnaowvni : buffer std_logic);
end um;

library ieee;
use ieee.std_logic_1164.all;

architecture fkbgktvsu of um is
  signal lzhlql : real;
  signal otzgsh : real;
  signal klsthn : std_logic_vector(0 downto 1);
  signal khxruarqs : std_logic_vector(0 downto 1);
  signal e : real;
begin
  v : entity work.uqtyfhe
    port map (ac => e, vnbby => tzbnaowvni, ebjg => khxruarqs);
  meve : entity work.cqqje
    port map (te => klsthn);
  xatjw : entity work.uqtyfhe
    port map (ac => otzgsh, vnbby => tzbnaowvni, ebjg => khxruarqs);
  ikchpijls : entity work.uqtyfhe
    port map (ac => lzhlql, vnbby => tzbnaowvni, ebjg => klsthn);
  
  -- Multi-driven assignments
  tzbnaowvni <= '1';
  khxruarqs <= khxruarqs;
end fkbgktvsu;



-- Seed after: 12665789619838071533,3316342841050048249
