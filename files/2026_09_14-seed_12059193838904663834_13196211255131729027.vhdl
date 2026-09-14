-- Seed: 12059193838904663834,13196211255131729027

library ieee;
use ieee.std_logic_1164.all;

entity ls is
  port (klwwlfmbcq : in std_logic_vector(0 to 1); etucwdsk : in real; vpmgnlpn : inout bit_vector(0 downto 2));
end ls;

architecture g of ls is
  
begin
  
end g;

entity rlmkxus is
  port (uoosnmyiq : inout time; bbydcijd : buffer time_vector(1 downto 4); uhkwcjgpr : out real);
end rlmkxus;

library ieee;
use ieee.std_logic_1164.all;

architecture jdjes of rlmkxus is
  signal ejojjpxb : bit_vector(0 downto 2);
  signal oipyph : real;
  signal mplmtdvio : bit_vector(0 downto 2);
  signal kptuayx : std_logic_vector(0 to 1);
  signal xzivgbgka : bit_vector(0 downto 2);
  signal umhoqqpf : real;
  signal wrxpkjimh : std_logic_vector(0 to 1);
begin
  ew : entity work.ls
    port map (klwwlfmbcq => wrxpkjimh, etucwdsk => umhoqqpf, vpmgnlpn => xzivgbgka);
  bgfuaqnt : entity work.ls
    port map (klwwlfmbcq => kptuayx, etucwdsk => umhoqqpf, vpmgnlpn => mplmtdvio);
  zcgqxcz : entity work.ls
    port map (klwwlfmbcq => wrxpkjimh, etucwdsk => oipyph, vpmgnlpn => ejojjpxb);
  
  -- Single-driven assignments
  uhkwcjgpr <= uhkwcjgpr;
  umhoqqpf <= uhkwcjgpr;
  oipyph <= uhkwcjgpr;
  bbydcijd <= bbydcijd;
  
  -- Multi-driven assignments
  kptuayx <= "U-";
  kptuayx <= ('L', 'X');
  kptuayx <= "00";
  wrxpkjimh <= ('U', '-');
end jdjes;



-- Seed after: 3314654527744490325,13196211255131729027
