-- Seed: 9746997970944248925,13196211255131729027

entity jnpmtrhd is
  port (ei : inout integer_vector(3 to 0));
end jnpmtrhd;

architecture hyacl of jnpmtrhd is
  
begin
  -- Single-driven assignments
  ei <= ei;
end hyacl;

library ieee;
use ieee.std_logic_1164.all;

entity yif is
  port (ciflf : buffer real; spkqczvow : buffer severity_level; naobuotf : in std_logic_vector(3 to 3); tqprirsxv : out time);
end yif;

architecture lapzmj of yif is
  signal wjyfbgipri : integer_vector(3 to 0);
begin
  ijxidoi : entity work.jnpmtrhd
    port map (ei => wjyfbgipri);
  
  -- Single-driven assignments
  tqprirsxv <= 8#0_0_5_6# fs;
end lapzmj;

entity slfcfgccg is
  port (dkvzayvq : in time; bhiwlb : inout integer);
end slfcfgccg;

library ieee;
use ieee.std_logic_1164.all;

architecture bnekqprcna of slfcfgccg is
  signal ybjl : integer_vector(3 to 0);
  signal zjehvi : time;
  signal sfzljn : std_logic_vector(3 to 3);
  signal an : severity_level;
  signal vnjsvy : real;
  signal kdxfdn : time;
  signal trvd : std_logic_vector(3 to 3);
  signal obruatmz : severity_level;
  signal nvosteqxyv : real;
  signal k : integer_vector(3 to 0);
begin
  eczbxjle : entity work.jnpmtrhd
    port map (ei => k);
  lpog : entity work.yif
    port map (ciflf => nvosteqxyv, spkqczvow => obruatmz, naobuotf => trvd, tqprirsxv => kdxfdn);
  yzdzzvsxnd : entity work.yif
    port map (ciflf => vnjsvy, spkqczvow => an, naobuotf => sfzljn, tqprirsxv => zjehvi);
  bxleepgpvd : entity work.jnpmtrhd
    port map (ei => ybjl);
  
  -- Single-driven assignments
  bhiwlb <= bhiwlb;
end bnekqprcna;



-- Seed after: 3327679909652393315,13196211255131729027
