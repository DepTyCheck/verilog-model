-- Seed: 8956619362236930657,3108530264173481209

entity dvrhani is
  port (iercf : out real; tsowker : buffer integer);
end dvrhani;

architecture cxohl of dvrhani is
  
begin
  -- Single-driven assignments
  tsowker <= 2#1_1#;
  iercf <= 2#1.1_0_0_0#;
end cxohl;

entity tnwgswvie is
  port (naylyqmgs : buffer time_vector(0 downto 0); ocqoj : inout integer; gjqswnvod : inout integer; pdpgrgc : buffer string(4 to 2));
end tnwgswvie;

architecture bvdx of tnwgswvie is
  signal yrliw : integer;
  signal awmcet : real;
  signal bjc : real;
begin
  gwuc : entity work.dvrhani
    port map (iercf => bjc, tsowker => ocqoj);
  kswyetl : entity work.dvrhani
    port map (iercf => awmcet, tsowker => yrliw);
  
  -- Single-driven assignments
  gjqswnvod <= 1133;
  naylyqmgs <= (others => 2_1_1_3_3 fs);
  pdpgrgc <= "";
end bvdx;

library ieee;
use ieee.std_logic_1164.all;

entity clv is
  port (z : out std_logic; mglx : buffer std_logic_vector(1 downto 4));
end clv;

architecture ilwejjdna of clv is
  
begin
  -- Multi-driven assignments
  mglx <= (others => '0');
end ilwejjdna;



-- Seed after: 15287545670538704061,3108530264173481209
