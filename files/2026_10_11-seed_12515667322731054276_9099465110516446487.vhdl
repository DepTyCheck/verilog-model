-- Seed: 12515667322731054276,9099465110516446487

library ieee;
use ieee.std_logic_1164.all;

entity qh is
  port ( maipbpyou : inout std_logic_vector(4 downto 4)
  ; mgygktrna : out bit_vector(3 downto 1)
  ; w : in std_logic
  ; jqa : buffer std_logic_vector(3 to 1)
  );
end qh;

architecture fjixome of qh is
  
begin
  -- Single-driven assignments
  mgygktrna <= ('0', '1', '1');
end fjixome;

entity cncvqdwbcb is
  port (iexrlxxo : linkage real; h : inout severity_level; w : out real_vector(4 to 1));
end cncvqdwbcb;

library ieee;
use ieee.std_logic_1164.all;

architecture burk of cncvqdwbcb is
  signal x : std_logic_vector(3 to 1);
  signal sam : std_logic;
  signal hgk : bit_vector(3 downto 1);
  signal u : std_logic_vector(4 downto 4);
  signal dpw : bit_vector(3 downto 1);
  signal ragi : std_logic_vector(3 to 1);
  signal kjrglfqu : std_logic;
  signal edhrcl : bit_vector(3 downto 1);
  signal tpwbscac : std_logic_vector(4 downto 4);
begin
  rqurw : entity work.qh
    port map (maipbpyou => tpwbscac, mgygktrna => edhrcl, w => kjrglfqu, jqa => ragi);
  r : entity work.qh
    port map (maipbpyou => tpwbscac, mgygktrna => dpw, w => kjrglfqu, jqa => ragi);
  sgestaw : entity work.qh
    port map (maipbpyou => u, mgygktrna => hgk, w => sam, jqa => x);
  
  -- Single-driven assignments
  w <= w;
  h <= h;
  
  -- Multi-driven assignments
  tpwbscac <= (others => 'U');
  x <= (others => '0');
  tpwbscac <= tpwbscac;
end burk;



-- Seed after: 4341023467306085558,9099465110516446487
