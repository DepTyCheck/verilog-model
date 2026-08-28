-- Seed: 5755756106459578637,7198033922882419595

entity jetaglha is
  port (uyewsz : inout integer; pefvoly : out integer);
end jetaglha;

architecture a of jetaglha is
  
begin
  -- Single-driven assignments
  pefvoly <= 1_0_3;
  uyewsz <= 3_2;
end a;

entity qe is
  port (ycui : out real; xypsd : inout integer; tkyhly : in time);
end qe;

architecture frl of qe is
  signal sgfpww : integer;
  signal fgqjzultc : integer;
begin
  soh : entity work.jetaglha
    port map (uyewsz => fgqjzultc, pefvoly => sgfpww);
  
  -- Single-driven assignments
  xypsd <= 4_2_0_1;
  ycui <= ycui;
end frl;

library ieee;
use ieee.std_logic_1164.all;

entity wzyg is
  port (saozqfv : buffer character; trfvq : buffer std_logic; etv : linkage time);
end wzyg;

architecture r of wzyg is
  signal ljfxshwmd : integer;
  signal ufkkczfa : integer;
  signal tqlsclm : time;
  signal yecmmuzi : integer;
  signal gpcejadt : real;
  signal ebhpdx : integer;
  signal azmffbrm : integer;
begin
  bmzbovf : entity work.jetaglha
    port map (uyewsz => azmffbrm, pefvoly => ebhpdx);
  isljjbj : entity work.qe
    port map (ycui => gpcejadt, xypsd => yecmmuzi, tkyhly => tqlsclm);
  ulyqtfh : entity work.jetaglha
    port map (uyewsz => ufkkczfa, pefvoly => ljfxshwmd);
  
  -- Single-driven assignments
  saozqfv <= 'x';
  tqlsclm <= 4_2 ps;
end r;



-- Seed after: 16540189601486719763,7198033922882419595
