-- Seed: 7141278103632964818,16188444798499499427

library ieee;
use ieee.std_logic_1164.all;

entity mow is
  port (pxoln : out time; qm : in time; tpamyklsbu : in time_vector(0 downto 3); nbgjft : linkage std_logic_vector(3 to 3));
end mow;

architecture gmevz of mow is
  
begin
  
end gmevz;

library ieee;
use ieee.std_logic_1164.all;

entity w is
  port (fmeoiq : out bit; oewecu : inout std_logic; fve : in real);
end w;

library ieee;
use ieee.std_logic_1164.all;

architecture nj of w is
  signal o : std_logic_vector(3 to 3);
  signal mzqoejxi : time;
  signal uia : time_vector(0 downto 3);
  signal muiteplbq : time;
  signal sgmh : time;
  signal nfdfgu : std_logic_vector(3 to 3);
  signal lwc : time_vector(0 downto 3);
  signal tvqveuqdys : time;
  signal salcss : time;
begin
  rxmprfrq : entity work.mow
    port map (pxoln => salcss, qm => tvqveuqdys, tpamyklsbu => lwc, nbgjft => nfdfgu);
  tih : entity work.mow
    port map (pxoln => sgmh, qm => muiteplbq, tpamyklsbu => uia, nbgjft => nfdfgu);
  cnxdxjhni : entity work.mow
    port map (pxoln => tvqveuqdys, qm => mzqoejxi, tpamyklsbu => uia, nbgjft => o);
  
  -- Single-driven assignments
  muiteplbq <= tvqveuqdys;
  
  -- Multi-driven assignments
  oewecu <= 'H';
  o <= nfdfgu;
end nj;

entity jh is
  port (jx : out time; xrfiymqstl : buffer real; sl : linkage real);
end jh;

library ieee;
use ieee.std_logic_1164.all;

architecture tgjp of jh is
  signal aqbf : time;
  signal qb : std_logic_vector(3 to 3);
  signal p : time_vector(0 downto 3);
  signal kuqxu : time;
  signal fff : time;
  signal hp : std_logic_vector(3 to 3);
  signal l : time_vector(0 downto 3);
  signal ubkfo : std_logic;
  signal rdxso : bit;
begin
  xpo : entity work.w
    port map (fmeoiq => rdxso, oewecu => ubkfo, fve => xrfiymqstl);
  xfgttl : entity work.mow
    port map (pxoln => jx, qm => jx, tpamyklsbu => l, nbgjft => hp);
  a : entity work.mow
    port map (pxoln => fff, qm => kuqxu, tpamyklsbu => p, nbgjft => qb);
  miutkjy : entity work.mow
    port map (pxoln => aqbf, qm => kuqxu, tpamyklsbu => l, nbgjft => hp);
  
  -- Single-driven assignments
  xrfiymqstl <= 8#41157.0_5_1#;
  p <= (others => 0 ns);
  kuqxu <= 1 min;
  l <= (others => 0 ns);
  
  -- Multi-driven assignments
  ubkfo <= '0';
end tgjp;



-- Seed after: 870673768331657946,16188444798499499427
