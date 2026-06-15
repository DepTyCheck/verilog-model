-- Seed: 1051585918494424841,1834764876137802293

entity hnxrjuoom is
  port (xgessgn : linkage boolean_vector(2 to 4));
end hnxrjuoom;

architecture bzhbjxx of hnxrjuoom is
  
begin
  
end bzhbjxx;

entity qjdsaddaf is
  port (gunqg : out integer);
end qjdsaddaf;

architecture tobtx of qjdsaddaf is
  
begin
  -- Single-driven assignments
  gunqg <= 2#0#;
end tobtx;

entity ahukwgo is
  port (saatp : out time);
end ahukwgo;

architecture kghusxerd of ahukwgo is
  
begin
  -- Single-driven assignments
  saatp <= 1 hr;
end kghusxerd;

entity lzqfzuw is
  port (iyh : linkage real; wxs : in real; ycxndestjx : inout integer; uqgfogrpz : buffer integer);
end lzqfzuw;

architecture xwauccum of lzqfzuw is
  signal pnewwthj : time;
  signal qq : boolean_vector(2 to 4);
  signal hwwibzs : time;
  signal jzbqfjzk : time;
begin
  ygaesimhan : entity work.ahukwgo
    port map (saatp => jzbqfjzk);
  eztgomruxl : entity work.ahukwgo
    port map (saatp => hwwibzs);
  c : entity work.hnxrjuoom
    port map (xgessgn => qq);
  gk : entity work.ahukwgo
    port map (saatp => pnewwthj);
  
  -- Single-driven assignments
  ycxndestjx <= 8#7_6_6_0_7#;
  uqgfogrpz <= 8#1_2#;
end xwauccum;



-- Seed after: 9507727700757286918,1834764876137802293
