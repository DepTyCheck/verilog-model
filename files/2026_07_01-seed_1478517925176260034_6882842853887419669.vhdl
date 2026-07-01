-- Seed: 1478517925176260034,6882842853887419669

entity jopjg is
  port (tnsojmjz : out string(3 to 1));
end jopjg;

architecture xteojtlxp of jopjg is
  
begin
  -- Single-driven assignments
  tnsojmjz <= "";
end xteojtlxp;

library ieee;
use ieee.std_logic_1164.all;

entity wnrkyxr is
  port (hyi : in std_logic_vector(2 to 4); fjsejvxg : out bit_vector(3 to 3));
end wnrkyxr;

architecture mop of wnrkyxr is
  signal aw : string(3 to 1);
begin
  e : entity work.jopjg
    port map (tnsojmjz => aw);
  
  -- Single-driven assignments
  fjsejvxg <= (others => '0');
end mop;

entity qcpqhotu is
  port (ak : inout real);
end qcpqhotu;

architecture fapf of qcpqhotu is
  
begin
  -- Single-driven assignments
  ak <= 4.0_2_4_2_2;
end fapf;

entity grzyaxbbdo is
  port (ifvsscedtn : in severity_level; bhwzq : out boolean);
end grzyaxbbdo;

library ieee;
use ieee.std_logic_1164.all;

architecture ajgyuln of grzyaxbbdo is
  signal bbshuwb : bit_vector(3 to 3);
  signal ukqlt : std_logic_vector(2 to 4);
begin
  moub : entity work.wnrkyxr
    port map (hyi => ukqlt, fjsejvxg => bbshuwb);
  
  -- Multi-driven assignments
  ukqlt <= ('-', '-', '1');
  ukqlt <= ('Z', 'Z', 'W');
end ajgyuln;



-- Seed after: 4812379596462992773,6882842853887419669
