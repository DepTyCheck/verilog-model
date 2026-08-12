-- Seed: 3071917764509871543,8412319452373742525

library ieee;
use ieee.std_logic_1164.all;

entity fzrnpl is
  port (jbx : out std_logic; eqxg : out integer; f : out std_logic_vector(4 downto 2));
end fzrnpl;

architecture tad of fzrnpl is
  
begin
  -- Single-driven assignments
  eqxg <= eqxg;
  
  -- Multi-driven assignments
  f <= "WW0";
  f <= ('H', 'U', 'W');
  f <= "W0U";
end tad;

library ieee;
use ieee.std_logic_1164.all;

entity zqklmxfssm is
  port (r : out std_logic_vector(4 downto 0); i : out time);
end zqklmxfssm;

library ieee;
use ieee.std_logic_1164.all;

architecture eunyfdr of zqklmxfssm is
  signal ueizy : integer;
  signal h : std_logic_vector(4 downto 2);
  signal m : integer;
  signal om : std_logic;
begin
  mgr : entity work.fzrnpl
    port map (jbx => om, eqxg => m, f => h);
  vi : entity work.fzrnpl
    port map (jbx => om, eqxg => ueizy, f => h);
end eunyfdr;

entity zmdnavzc is
  port (mrnybitm : linkage boolean_vector(0 to 2));
end zmdnavzc;

library ieee;
use ieee.std_logic_1164.all;

architecture dhcidygcol of zmdnavzc is
  signal mqxdq : std_logic_vector(4 downto 2);
  signal wlv : integer;
  signal uonzelvu : std_logic;
begin
  jkmem : entity work.fzrnpl
    port map (jbx => uonzelvu, eqxg => wlv, f => mqxdq);
  
  -- Multi-driven assignments
  uonzelvu <= '1';
  uonzelvu <= '1';
  uonzelvu <= uonzelvu;
end dhcidygcol;

entity ignguqh is
  port (swjzutyp : buffer real; khmg : linkage time);
end ignguqh;

library ieee;
use ieee.std_logic_1164.all;

architecture xnrl of ignguqh is
  signal h : boolean_vector(0 to 2);
  signal ehgdpmv : integer;
  signal s : std_logic;
  signal irlud : std_logic_vector(4 downto 2);
  signal dv : integer;
  signal mkak : std_logic;
  signal xjuthjxp : boolean_vector(0 to 2);
begin
  v : entity work.zmdnavzc
    port map (mrnybitm => xjuthjxp);
  odzumij : entity work.fzrnpl
    port map (jbx => mkak, eqxg => dv, f => irlud);
  ws : entity work.fzrnpl
    port map (jbx => s, eqxg => ehgdpmv, f => irlud);
  jorbwga : entity work.zmdnavzc
    port map (mrnybitm => h);
  
  -- Single-driven assignments
  swjzutyp <= 2120.44;
  
  -- Multi-driven assignments
  mkak <= '1';
  mkak <= 'Z';
  s <= 'Z';
  irlud <= ('W', '1', 'H');
end xnrl;



-- Seed after: 778651693989974070,8412319452373742525
