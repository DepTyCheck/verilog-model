-- Seed: 15699174074474355864,17219977430298397867

library ieee;
use ieee.std_logic_1164.all;

entity lqsbvnxm is
  port (kefenlket : in boolean_vector(4 downto 1); qoyewi : in string(5 to 3); eza : inout std_logic_vector(4 downto 4); dewyic : out std_logic);
end lqsbvnxm;

architecture t of lqsbvnxm is
  
begin
  -- Multi-driven assignments
  dewyic <= dewyic;
  dewyic <= 'U';
  dewyic <= dewyic;
end t;

library ieee;
use ieee.std_logic_1164.all;

entity tdcsvrjdr is
  port (bo : out std_logic; eaxlhxtvmc : out time; kcvkmtlt : in time);
end tdcsvrjdr;

library ieee;
use ieee.std_logic_1164.all;

architecture hfihol of tdcsvrjdr is
  signal vroylfa : std_logic;
  signal yg : string(5 to 3);
  signal tjxf : std_logic_vector(4 downto 4);
  signal g : string(5 to 3);
  signal hmpfrrwil : boolean_vector(4 downto 1);
begin
  umu : entity work.lqsbvnxm
    port map (kefenlket => hmpfrrwil, qoyewi => g, eza => tjxf, dewyic => bo);
  y : entity work.lqsbvnxm
    port map (kefenlket => hmpfrrwil, qoyewi => yg, eza => tjxf, dewyic => vroylfa);
  
  -- Multi-driven assignments
  tjxf <= (others => 'W');
  bo <= bo;
end hfihol;



-- Seed after: 13519753176891027560,17219977430298397867
