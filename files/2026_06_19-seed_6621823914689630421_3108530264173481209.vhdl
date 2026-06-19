-- Seed: 6621823914689630421,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity kfzhf is
  port (tkpjndg : inout integer; iok : out time; xxw : inout std_logic; vqcndn : buffer real);
end kfzhf;

architecture ddzusovvw of kfzhf is
  
begin
  -- Single-driven assignments
  tkpjndg <= 16#8_B#;
  iok <= 8#5.0_6_5_0_4# fs;
  
  -- Multi-driven assignments
  xxw <= 'Z';
  xxw <= '0';
  xxw <= 'W';
end ddzusovvw;

library ieee;
use ieee.std_logic_1164.all;

entity khsi is
  port (dux : linkage std_logic; cwcymaowls : out bit);
end khsi;

library ieee;
use ieee.std_logic_1164.all;

architecture tvsmy of khsi is
  signal vyuhucj : real;
  signal vl : time;
  signal dngy : integer;
  signal xn : real;
  signal ddymem : time;
  signal bfyniic : integer;
  signal sk : real;
  signal wfw : time;
  signal kkoghf : integer;
  signal k : real;
  signal rqw : std_logic;
  signal saefkvdxw : time;
  signal vnbhavk : integer;
begin
  ke : entity work.kfzhf
    port map (tkpjndg => vnbhavk, iok => saefkvdxw, xxw => rqw, vqcndn => k);
  xjiacjx : entity work.kfzhf
    port map (tkpjndg => kkoghf, iok => wfw, xxw => rqw, vqcndn => sk);
  lovjsmezr : entity work.kfzhf
    port map (tkpjndg => bfyniic, iok => ddymem, xxw => rqw, vqcndn => xn);
  gzp : entity work.kfzhf
    port map (tkpjndg => dngy, iok => vl, xxw => rqw, vqcndn => vyuhucj);
  
  -- Single-driven assignments
  cwcymaowls <= '0';
  
  -- Multi-driven assignments
  rqw <= 'W';
  rqw <= 'Z';
  rqw <= 'Z';
end tvsmy;



-- Seed after: 5380689677478620028,3108530264173481209
