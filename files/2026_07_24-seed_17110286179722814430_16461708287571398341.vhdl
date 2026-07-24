-- Seed: 17110286179722814430,16461708287571398341

entity y is
  port (tk : out bit_vector(3 downto 1); nrnx : buffer integer; pke : buffer bit);
end y;

architecture s of y is
  
begin
  -- Single-driven assignments
  pke <= '1';
  tk <= tk;
end s;

entity a is
  port (uay : linkage boolean; gdojbva : in time; unfjp : out time);
end a;

architecture ohvwt of a is
  signal igpohte : bit;
  signal ka : integer;
  signal gr : bit_vector(3 downto 1);
  signal nsikcltwx : bit;
  signal nbsbjcbbiu : integer;
  signal lbceel : bit_vector(3 downto 1);
begin
  ttidhfiz : entity work.y
    port map (tk => lbceel, nrnx => nbsbjcbbiu, pke => nsikcltwx);
  k : entity work.y
    port map (tk => gr, nrnx => ka, pke => igpohte);
  
  -- Single-driven assignments
  unfjp <= unfjp;
end ohvwt;

library ieee;
use ieee.std_logic_1164.all;

entity iik is
  port (d : buffer integer_vector(4 to 4); tuh : in real; pxu : in std_logic);
end iik;

architecture uoehsr of iik is
  signal pnfraia : bit;
  signal vyoq : integer;
  signal ssnr : bit_vector(3 downto 1);
  signal vbue : bit;
  signal xhirbqunn : integer;
  signal sd : bit_vector(3 downto 1);
begin
  xpxf : entity work.y
    port map (tk => sd, nrnx => xhirbqunn, pke => vbue);
  m : entity work.y
    port map (tk => ssnr, nrnx => vyoq, pke => pnfraia);
  
  -- Single-driven assignments
  d <= (others => 2_4_3_0_2);
end uoehsr;

library ieee;
use ieee.std_logic_1164.all;

entity gcwa is
  port (dfooocxhn : in time; lmbgkk : in std_logic; hxntkcrx : inout std_logic);
end gcwa;

architecture yfvenmjy of gcwa is
  signal fic : bit;
  signal gqqc : integer;
  signal fdwxsgo : bit_vector(3 downto 1);
  signal s : real;
  signal e : integer_vector(4 to 4);
  signal kuoiijbung : bit;
  signal yj : integer;
  signal wr : bit_vector(3 downto 1);
begin
  ulkosjis : entity work.y
    port map (tk => wr, nrnx => yj, pke => kuoiijbung);
  rg : entity work.iik
    port map (d => e, tuh => s, pxu => hxntkcrx);
  xpo : entity work.y
    port map (tk => fdwxsgo, nrnx => gqqc, pke => fic);
  
  -- Single-driven assignments
  s <= s;
  
  -- Multi-driven assignments
  hxntkcrx <= hxntkcrx;
  hxntkcrx <= 'H';
  hxntkcrx <= '-';
end yfvenmjy;



-- Seed after: 17956628434050870203,16461708287571398341
