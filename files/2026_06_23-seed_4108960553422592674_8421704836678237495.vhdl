-- Seed: 4108960553422592674,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity d is
  port (zng : linkage std_logic_vector(2 downto 2); qijercf : out real);
end d;

architecture gcuvots of d is
  
begin
  -- Single-driven assignments
  qijercf <= 0_2.3013;
end gcuvots;

library ieee;
use ieee.std_logic_1164.all;

entity jnfcdwrg is
  port (cxjbnu : linkage std_logic; jlrwnayg : buffer severity_level);
end jnfcdwrg;

library ieee;
use ieee.std_logic_1164.all;

architecture lvmems of jnfcdwrg is
  signal ttvxbb : real;
  signal fkdz : std_logic_vector(2 downto 2);
  signal afru : real;
  signal nasusocht : std_logic_vector(2 downto 2);
  signal hgwpu : real;
  signal z : std_logic_vector(2 downto 2);
  signal jnirel : real;
  signal jraqq : std_logic_vector(2 downto 2);
begin
  zszunifoq : entity work.d
    port map (zng => jraqq, qijercf => jnirel);
  q : entity work.d
    port map (zng => z, qijercf => hgwpu);
  nop : entity work.d
    port map (zng => nasusocht, qijercf => afru);
  ljuxkkntb : entity work.d
    port map (zng => fkdz, qijercf => ttvxbb);
  
  -- Single-driven assignments
  jlrwnayg <= FAILURE;
  
  -- Multi-driven assignments
  fkdz <= (others => 'X');
  jraqq <= (others => '-');
  jraqq <= (others => 'L');
  jraqq <= (others => 'Z');
end lvmems;



-- Seed after: 12827291422212942134,8421704836678237495
