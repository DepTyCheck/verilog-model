-- Seed: 17744736317199569631,17234720251424330329

library ieee;
use ieee.std_logic_1164.all;

entity vbfms is
  port (nnbwwmjh : buffer std_logic; xixhsced : in real; lqqjix : inout real);
end vbfms;

architecture gcz of vbfms is
  
begin
  -- Single-driven assignments
  lqqjix <= 3114.1_2_3_3;
  
  -- Multi-driven assignments
  nnbwwmjh <= '0';
  nnbwwmjh <= nnbwwmjh;
  nnbwwmjh <= 'X';
  nnbwwmjh <= 'Z';
end gcz;

entity ulfc is
  port (nvvbbsfxt : out string(2 to 3); lg : out real; qh : buffer real);
end ulfc;

library ieee;
use ieee.std_logic_1164.all;

architecture t of ulfc is
  signal rppskoaze : real;
  signal ksuatkz : real;
  signal gpmzk : real;
  signal wp : std_logic;
begin
  e : entity work.vbfms
    port map (nnbwwmjh => wp, xixhsced => gpmzk, lqqjix => ksuatkz);
  xaukrqtih : entity work.vbfms
    port map (nnbwwmjh => wp, xixhsced => qh, lqqjix => rppskoaze);
  
  -- Single-driven assignments
  qh <= lg;
  nvvbbsfxt <= "gk";
  gpmzk <= 8#6.1#;
  lg <= 0_3.3_3;
  
  -- Multi-driven assignments
  wp <= 'U';
end t;

library ieee;
use ieee.std_logic_1164.all;

entity jbzpml is
  port (c : out character; vmoyduh : out std_logic; mbb : out real);
end jbzpml;

library ieee;
use ieee.std_logic_1164.all;

architecture iapw of jbzpml is
  signal ilr : real;
  signal hgxdjr : real;
  signal cjzlzn : string(2 to 3);
  signal tdgsehqxbc : real;
  signal snalxyffzi : real;
  signal rejvu : real;
  signal ezfjiz : std_logic;
begin
  mpydvsgmkc : entity work.vbfms
    port map (nnbwwmjh => ezfjiz, xixhsced => mbb, lqqjix => rejvu);
  gdhrwqden : entity work.vbfms
    port map (nnbwwmjh => vmoyduh, xixhsced => snalxyffzi, lqqjix => tdgsehqxbc);
  lbjlwau : entity work.ulfc
    port map (nvvbbsfxt => cjzlzn, lg => mbb, qh => hgxdjr);
  mkqa : entity work.vbfms
    port map (nnbwwmjh => ezfjiz, xixhsced => mbb, lqqjix => ilr);
  
  -- Single-driven assignments
  c <= c;
  snalxyffzi <= mbb;
end iapw;



-- Seed after: 5801126161285573384,17234720251424330329
