-- Seed: 3083343812102569274,3042374792655995433

entity xba is
  port (vocyerc : buffer time; ngoseauib : out bit_vector(1 downto 4); xkg : linkage bit_vector(0 downto 3));
end xba;

architecture dgztjyhbg of xba is
  
begin
  
end dgztjyhbg;

entity b is
  port (ci : buffer real; dzyawjghx : out time; kp : inout bit);
end b;

architecture v of b is
  signal zh : bit_vector(0 downto 3);
  signal z : bit_vector(1 downto 4);
  signal rezxillwru : time;
  signal gkqg : bit_vector(0 downto 3);
  signal bvu : bit_vector(1 downto 4);
begin
  h : entity work.xba
    port map (vocyerc => dzyawjghx, ngoseauib => bvu, xkg => gkqg);
  mobb : entity work.xba
    port map (vocyerc => rezxillwru, ngoseauib => z, xkg => zh);
  
  -- Single-driven assignments
  kp <= '0';
  ci <= 16#6_7.61#;
end v;

entity lu is
  port (mvtejodui : buffer boolean_vector(0 downto 3); xdxkyqg : in bit; eld : buffer integer; ye : buffer real);
end lu;

architecture jbrp of lu is
  signal cdid : bit_vector(0 downto 3);
  signal rdpcsd : bit_vector(1 downto 4);
  signal btrr : time;
begin
  xpyd : entity work.xba
    port map (vocyerc => btrr, ngoseauib => rdpcsd, xkg => cdid);
end jbrp;



-- Seed after: 16192401438991490559,3042374792655995433
