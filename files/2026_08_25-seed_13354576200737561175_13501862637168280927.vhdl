-- Seed: 13354576200737561175,13501862637168280927

entity suts is
  port (ldae : buffer integer; ykgsajcyq : buffer real; tophcgo : buffer time; xv : out real);
end suts;

architecture k of suts is
  
begin
  -- Single-driven assignments
  xv <= xv;
  ykgsajcyq <= xv;
  tophcgo <= tophcgo;
end k;

entity f is
  port (o : inout time);
end f;

architecture awhbrq of f is
  
begin
  -- Single-driven assignments
  o <= 16#72E46# us;
end awhbrq;

library ieee;
use ieee.std_logic_1164.all;

entity ji is
  port (fwoxey : out time; hdmcaoryk : inout std_logic);
end ji;

architecture zakgujfeu of ji is
  signal hyqokzqs : real;
  signal tejtcfcwg : time;
  signal zkivfaglls : real;
  signal iqzdwz : integer;
  signal gu : real;
  signal cwivcjk : real;
  signal almcjsd : integer;
  signal gbz : time;
begin
  dso : entity work.f
    port map (o => gbz);
  tlnnj : entity work.suts
    port map (ldae => almcjsd, ykgsajcyq => cwivcjk, tophcgo => fwoxey, xv => gu);
  mkbybqpdgt : entity work.suts
    port map (ldae => iqzdwz, ykgsajcyq => zkivfaglls, tophcgo => tejtcfcwg, xv => hyqokzqs);
  
  -- Multi-driven assignments
  hdmcaoryk <= 'L';
  hdmcaoryk <= 'U';
  hdmcaoryk <= hdmcaoryk;
  hdmcaoryk <= hdmcaoryk;
end zakgujfeu;



-- Seed after: 11930889107775814089,13501862637168280927
