-- Seed: 1539240360238061008,7304262412290825129

entity xyuguhvhfa is
  port (k : out integer; uvqrmhc : in bit_vector(4 downto 0); kdtcw : inout time);
end xyuguhvhfa;

architecture hywgnhk of xyuguhvhfa is
  
begin
  -- Single-driven assignments
  kdtcw <= 2#1001# fs;
  k <= 8#3_4#;
end hywgnhk;

entity svdqaupp is
  port (zluovvjhyw : in severity_level; ceaorsw : out bit);
end svdqaupp;

architecture c of svdqaupp is
  signal y : time;
  signal yzswrqx : bit_vector(4 downto 0);
  signal m : integer;
begin
  dwgzrdb : entity work.xyuguhvhfa
    port map (k => m, uvqrmhc => yzswrqx, kdtcw => y);
  
  -- Single-driven assignments
  ceaorsw <= ceaorsw;
  yzswrqx <= ('1', '1', '0', '0', '1');
end c;



-- Seed after: 14243020020654218692,7304262412290825129
