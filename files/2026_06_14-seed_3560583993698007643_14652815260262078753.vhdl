-- Seed: 3560583993698007643,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity tspftvnotm is
  port (jewec : inout severity_level; nasx : inout std_logic_vector(4 to 4));
end tspftvnotm;

architecture i of tspftvnotm is
  
begin
  -- Single-driven assignments
  jewec <= NOTE;
  
  -- Multi-driven assignments
  nasx <= "L";
end i;

entity qtexzsue is
  port (zhuvvpo : buffer bit_vector(4 to 1));
end qtexzsue;

architecture mdl of qtexzsue is
  
begin
  -- Single-driven assignments
  zhuvvpo <= (others => '0');
end mdl;

library ieee;
use ieee.std_logic_1164.all;

entity feoi is
  port (toa : linkage time_vector(4 downto 0); avepteo : buffer std_logic_vector(4 to 4); xxe : buffer string(3 to 3));
end feoi;

architecture tevkprbqhl of feoi is
  signal uykyyl : severity_level;
begin
  ryizj : entity work.tspftvnotm
    port map (jewec => uykyyl, nasx => avepteo);
  
  -- Single-driven assignments
  xxe <= (others => 'z');
  
  -- Multi-driven assignments
  avepteo <= "H";
  avepteo <= (others => 'X');
  avepteo <= "-";
  avepteo <= (others => 'X');
end tevkprbqhl;



-- Seed after: 3781277381881148082,14652815260262078753
