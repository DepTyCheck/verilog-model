-- Seed: 16304437467363930535,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity fbsfxl is
  port (qipknxwzzt : out time; fwzpgclmeo : inout std_logic);
end fbsfxl;

architecture qcybajzbde of fbsfxl is
  
begin
  -- Single-driven assignments
  qipknxwzzt <= 16#4982# ms;
  
  -- Multi-driven assignments
  fwzpgclmeo <= '0';
  fwzpgclmeo <= '-';
  fwzpgclmeo <= '-';
end qcybajzbde;

entity zhnusw is
  port (orfzrltk : in integer; qaaxuaouk : inout time_vector(1 downto 3); h : out severity_level);
end zhnusw;

architecture aflkk of zhnusw is
  
begin
  -- Single-driven assignments
  h <= FAILURE;
  qaaxuaouk <= (others => 0 ns);
end aflkk;

library ieee;
use ieee.std_logic_1164.all;

entity ci is
  port (cijlegavi : buffer std_logic_vector(2 downto 0); ayxuewik : buffer real_vector(0 to 3); nsiumt : buffer bit_vector(2 to 3));
end ci;

architecture sw of ci is
  signal pvi : severity_level;
  signal nemkthmzo : time_vector(1 downto 3);
  signal lwxptyvxc : integer;
begin
  yplngqjcm : entity work.zhnusw
    port map (orfzrltk => lwxptyvxc, qaaxuaouk => nemkthmzo, h => pvi);
  
  -- Single-driven assignments
  nsiumt <= ('1', '1');
  
  -- Multi-driven assignments
  cijlegavi <= ('H', 'X', 'X');
  cijlegavi <= ('1', 'Z', 'H');
  cijlegavi <= ('H', 'L', 'Z');
  cijlegavi <= ('W', '0', '1');
end sw;

entity e is
  port (afkvef : inout integer; jmsjjsed : linkage severity_level; b : inout real);
end e;

library ieee;
use ieee.std_logic_1164.all;

architecture zvwtt of e is
  signal u : std_logic;
  signal lijeumrjcw : time;
begin
  hjnjklqpmd : entity work.fbsfxl
    port map (qipknxwzzt => lijeumrjcw, fwzpgclmeo => u);
  
  -- Single-driven assignments
  afkvef <= 8#602#;
  b <= 2#0_0.0#;
  
  -- Multi-driven assignments
  u <= '-';
  u <= '1';
end zvwtt;



-- Seed after: 10891291056841376675,8421704836678237495
