-- Seed: 5188244155370488924,6379010654866854599

library ieee;
use ieee.std_logic_1164.all;

entity yovbvi is
  port (n : inout std_logic_vector(2 to 1); z : inout integer);
end yovbvi;

architecture gcl of yovbvi is
  
begin
  -- Single-driven assignments
  z <= 16#53#;
  
  -- Multi-driven assignments
  n <= n;
  n <= "";
  n <= (others => '0');
  n <= n;
end gcl;

entity yahiknwovt is
  port (rvwvnzrumc : out real; yvdreghx : buffer integer; moahn : inout severity_level; isau : out time);
end yahiknwovt;

library ieee;
use ieee.std_logic_1164.all;

architecture kg of yahiknwovt is
  signal ajqqul : integer;
  signal aazuu : integer;
  signal scdydnqea : std_logic_vector(2 to 1);
  signal aozrfzqlqu : std_logic_vector(2 to 1);
  signal bpfdnxcsnv : integer;
  signal mflzeqta : std_logic_vector(2 to 1);
begin
  bep : entity work.yovbvi
    port map (n => mflzeqta, z => bpfdnxcsnv);
  rnftdvq : entity work.yovbvi
    port map (n => aozrfzqlqu, z => yvdreghx);
  hhr : entity work.yovbvi
    port map (n => scdydnqea, z => aazuu);
  voy : entity work.yovbvi
    port map (n => mflzeqta, z => ajqqul);
  
  -- Single-driven assignments
  isau <= isau;
end kg;



-- Seed after: 9497331813656357105,6379010654866854599
