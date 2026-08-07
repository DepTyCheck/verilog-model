-- Seed: 11631363234925354197,8068158652091157513

library ieee;
use ieee.std_logic_1164.all;

entity itpzwr is
  port (asxecwbgl : in std_logic; omutm : buffer boolean_vector(3 downto 4); klz : in time_vector(4 to 0); xipimad : inout integer);
end itpzwr;

architecture yil of itpzwr is
  
begin
  -- Single-driven assignments
  xipimad <= 42103;
  omutm <= (others => TRUE);
end yil;

entity ycvq is
  port (ocptvpifz : buffer real);
end ycvq;

library ieee;
use ieee.std_logic_1164.all;

architecture kezpunko of ycvq is
  signal palfmps : integer;
  signal qladhapwzv : time_vector(4 to 0);
  signal sotdhscfi : boolean_vector(3 downto 4);
  signal noapamau : std_logic;
  signal hukkqqtubb : integer;
  signal vimofux : time_vector(4 to 0);
  signal bwwlcksgun : boolean_vector(3 downto 4);
  signal e : integer;
  signal mryuhkpuk : time_vector(4 to 0);
  signal vqqff : boolean_vector(3 downto 4);
  signal yxs : std_logic;
begin
  basgmzna : entity work.itpzwr
    port map (asxecwbgl => yxs, omutm => vqqff, klz => mryuhkpuk, xipimad => e);
  vv : entity work.itpzwr
    port map (asxecwbgl => yxs, omutm => bwwlcksgun, klz => vimofux, xipimad => hukkqqtubb);
  ir : entity work.itpzwr
    port map (asxecwbgl => noapamau, omutm => sotdhscfi, klz => qladhapwzv, xipimad => palfmps);
end kezpunko;



-- Seed after: 14456229017078101433,8068158652091157513
