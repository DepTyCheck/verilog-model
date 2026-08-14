-- Seed: 1180646569818618388,8437298063418820479

entity gzkr is
  port (nmthpg : buffer bit_vector(1 to 4); pmy : out time; kh : inout time);
end gzkr;

architecture cb of gzkr is
  
begin
  -- Single-driven assignments
  pmy <= kh;
end cb;

entity ijqjgycbpn is
  port (nenmicsj : inout bit);
end ijqjgycbpn;

architecture bgjyeabpg of ijqjgycbpn is
  signal dnqseec : time;
  signal hwmsblyh : time;
  signal pyixf : bit_vector(1 to 4);
  signal ulfussko : time;
  signal ppzhzfy : time;
  signal tebsuzx : bit_vector(1 to 4);
begin
  hcmjolil : entity work.gzkr
    port map (nmthpg => tebsuzx, pmy => ppzhzfy, kh => ulfussko);
  paooinj : entity work.gzkr
    port map (nmthpg => pyixf, pmy => hwmsblyh, kh => dnqseec);
  
  -- Single-driven assignments
  nenmicsj <= nenmicsj;
end bgjyeabpg;

library ieee;
use ieee.std_logic_1164.all;

entity uokknhd is
  port (krkrykn : inout std_logic_vector(2 downto 3));
end uokknhd;

architecture rmtmbqhnjj of uokknhd is
  signal oiopspr : bit;
begin
  tqlutuictd : entity work.ijqjgycbpn
    port map (nenmicsj => oiopspr);
end rmtmbqhnjj;



-- Seed after: 8974878570280558400,8437298063418820479
