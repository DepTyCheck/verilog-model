-- Seed: 5260179246553109828,3181554006726329157

entity falz is
  port (i : buffer boolean_vector(1 downto 2));
end falz;

architecture iokbtf of falz is
  
begin
  -- Single-driven assignments
  i <= (others => TRUE);
end iokbtf;

use std.reflection.all;

entity grhbpbb is
  port (wkiq : inout physical_subtype_mirror; femhosnn : linkage integer);
end grhbpbb;

architecture zhym of grhbpbb is
  signal l : boolean_vector(1 downto 2);
  signal vapkbouzvs : boolean_vector(1 downto 2);
  signal hluhcd : boolean_vector(1 downto 2);
  signal qopuolje : boolean_vector(1 downto 2);
begin
  vmxp : entity work.falz
    port map (i => qopuolje);
  meddm : entity work.falz
    port map (i => hluhcd);
  obrvmfnh : entity work.falz
    port map (i => vapkbouzvs);
  zvb : entity work.falz
    port map (i => l);
end zhym;

use std.reflection.all;

entity rxzhvkt is
  port (xaoxzo : inout physical_value_mirror; zhdwg : inout array_subtype_mirror);
end rxzhvkt;

architecture wqqqebg of rxzhvkt is
  signal yvgefveb : boolean_vector(1 downto 2);
begin
  dscg : entity work.falz
    port map (i => yvgefveb);
end wqqqebg;



-- Seed after: 871896211295941795,3181554006726329157
