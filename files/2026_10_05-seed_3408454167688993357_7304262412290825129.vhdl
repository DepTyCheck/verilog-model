-- Seed: 3408454167688993357,7304262412290825129

library ieee;
use ieee.std_logic_1164.all;

entity pnham is
  port (gunr : linkage bit_vector(0 to 4); e : out real_vector(2 downto 0); sofcnvfx : linkage std_logic; kn : buffer time_vector(4 downto 3));
end pnham;

architecture xgud of pnham is
  
begin
  -- Single-driven assignments
  kn <= (023.211 ps, 0 min);
  e <= (244.1_3_3_3_1, 233.0, 021.3);
end xgud;

entity vdgicbze is
  port (dlnpbv : inout boolean);
end vdgicbze;

library ieee;
use ieee.std_logic_1164.all;

architecture ahh of vdgicbze is
  signal qrfxgpqrle : time_vector(4 downto 3);
  signal sva : real_vector(2 downto 0);
  signal rrexvt : bit_vector(0 to 4);
  signal ybflt : time_vector(4 downto 3);
  signal cngrdo : real_vector(2 downto 0);
  signal yfnrmse : bit_vector(0 to 4);
  signal umpqvoorxn : time_vector(4 downto 3);
  signal v : std_logic;
  signal oabpxnwhri : real_vector(2 downto 0);
  signal szysogan : bit_vector(0 to 4);
begin
  gpogj : entity work.pnham
    port map (gunr => szysogan, e => oabpxnwhri, sofcnvfx => v, kn => umpqvoorxn);
  t : entity work.pnham
    port map (gunr => yfnrmse, e => cngrdo, sofcnvfx => v, kn => ybflt);
  prp : entity work.pnham
    port map (gunr => rrexvt, e => sva, sofcnvfx => v, kn => qrfxgpqrle);
  
  -- Single-driven assignments
  dlnpbv <= dlnpbv;
  
  -- Multi-driven assignments
  v <= v;
  v <= 'H';
end ahh;



-- Seed after: 17780891710384750500,7304262412290825129
