-- Seed: 291040988010262558,13196211255131729027

entity vatdq is
  port (ctkejg : out real);
end vatdq;

architecture wysm of vatdq is
  
begin
  -- Single-driven assignments
  ctkejg <= 16#0F932.6_4_5#;
end wysm;

entity ec is
  port (nveiw : in time; npnkaap : out time; twholfnp : buffer integer_vector(4 downto 0); zibtfmhoz : in string(2 to 1));
end ec;

architecture cdqv of ec is
  signal k : real;
  signal ntyaxvmh : real;
  signal eiz : real;
begin
  ezltltzzr : entity work.vatdq
    port map (ctkejg => eiz);
  nw : entity work.vatdq
    port map (ctkejg => ntyaxvmh);
  vbr : entity work.vatdq
    port map (ctkejg => k);
  
  -- Single-driven assignments
  twholfnp <= (16#F_C#, 2#1000#, 16#F_D_8#, 1, 2#0#);
  npnkaap <= 2_4_1.10 ms;
end cdqv;

entity xoznnef is
  port (xyjnms : inout bit; jjtndjvfw : out character);
end xoznnef;

architecture rvsoes of xoznnef is
  signal blxvqg : real;
  signal vzzj : string(2 to 1);
  signal syflcwnssw : integer_vector(4 downto 0);
  signal jeb : time;
begin
  fmt : entity work.ec
    port map (nveiw => jeb, npnkaap => jeb, twholfnp => syflcwnssw, zibtfmhoz => vzzj);
  z : entity work.vatdq
    port map (ctkejg => blxvqg);
  
  -- Single-driven assignments
  jjtndjvfw <= 'e';
end rvsoes;



-- Seed after: 18366447615684322257,13196211255131729027
