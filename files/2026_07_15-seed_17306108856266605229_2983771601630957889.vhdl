-- Seed: 17306108856266605229,2983771601630957889

entity usqghj is
  port (dgapju : buffer integer; j : in bit_vector(1 downto 2));
end usqghj;

architecture sfu of usqghj is
  
begin
  -- Single-driven assignments
  dgapju <= 16#F059E#;
end sfu;

use std.reflection.all;

entity pvaii is
  port (variable judlwlmkks : inout protected_value_mirror_pt);
end pvaii;

architecture hutkcet of pvaii is
  signal pmjblmqgim : bit_vector(1 downto 2);
  signal iugz : integer;
  signal ja : bit_vector(1 downto 2);
  signal lnflk : integer;
  signal zwqfb : integer;
  signal azanrrykqr : bit_vector(1 downto 2);
  signal zhgflr : integer;
begin
  kxxtkjpdr : entity work.usqghj
    port map (dgapju => zhgflr, j => azanrrykqr);
  bkumi : entity work.usqghj
    port map (dgapju => zwqfb, j => azanrrykqr);
  ywsugifg : entity work.usqghj
    port map (dgapju => lnflk, j => ja);
  hxlswie : entity work.usqghj
    port map (dgapju => iugz, j => pmjblmqgim);
  
  -- Single-driven assignments
  azanrrykqr <= azanrrykqr;
  pmjblmqgim <= (others => '0');
  ja <= azanrrykqr;
end hutkcet;

use std.reflection.all;

entity fchjxkajw is
  port (variable e : inout integer_value_mirror_pt; variable x : inout physical_subtype_mirror_pt; variable ydfkfbey : inout array_subtype_mirror_pt);
end fchjxkajw;

use std.reflection.all;

architecture yjm of fchjxkajw is
  signal s : integer;
  shared variable ows : protected_value_mirror_pt;
  signal entmrriied : bit_vector(1 downto 2);
  signal nbmdhiqnp : integer;
  shared variable xmvuk : protected_value_mirror_pt;
begin
  oywrcfzsux : entity work.pvaii
    port map (judlwlmkks => xmvuk);
  inl : entity work.usqghj
    port map (dgapju => nbmdhiqnp, j => entmrriied);
  l : entity work.pvaii
    port map (judlwlmkks => ows);
  zgdmcfr : entity work.usqghj
    port map (dgapju => s, j => entmrriied);
end yjm;



-- Seed after: 15342183183861261646,2983771601630957889
