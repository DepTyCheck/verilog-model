-- Seed: 13633173714079592826,8118127366649987907

entity hkjmmyi is
  port (jfp : in integer_vector(2 downto 4));
end hkjmmyi;

architecture wfeevvxt of hkjmmyi is
  
begin
  
end wfeevvxt;

entity ac is
  port (wsmuasucxj : inout time_vector(4 to 0));
end ac;

architecture ule of ac is
  signal ki : integer_vector(2 downto 4);
begin
  pjgzvljsg : entity work.hkjmmyi
    port map (jfp => ki);
  
  -- Single-driven assignments
  wsmuasucxj <= (others => 0 ns);
  ki <= (others => 0);
end ule;

entity jw is
  port (mxou : buffer bit; zz : out integer; mzjkfygccy : linkage integer; zsl : out bit_vector(2 downto 2));
end jw;

architecture iavaicugy of jw is
  signal fdggagjo : integer_vector(2 downto 4);
begin
  zft : entity work.hkjmmyi
    port map (jfp => fdggagjo);
  
  -- Single-driven assignments
  zz <= 16#9_2_C#;
  fdggagjo <= (others => 0);
  zsl <= (others => '1');
end iavaicugy;



-- Seed after: 14494265776745130367,8118127366649987907
