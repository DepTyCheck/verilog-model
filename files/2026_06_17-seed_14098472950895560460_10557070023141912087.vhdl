-- Seed: 14098472950895560460,10557070023141912087

entity mab is
  port (obstatmq : out real; npgt : out integer; yc : buffer real);
end mab;

architecture bikrpogy of mab is
  
begin
  -- Single-driven assignments
  yc <= 2#1_1_1.0_1_0_1#;
  obstatmq <= 2_1.412;
  npgt <= 2_2_2_1;
end bikrpogy;

entity xnhnep is
  port (r : in integer);
end xnhnep;

architecture zqtarretdg of xnhnep is
  signal bvwcedcoaf : real;
  signal s : integer;
  signal mzqj : real;
  signal e : real;
  signal tfaowdziej : integer;
  signal nnhbb : real;
  signal punxj : real;
  signal ywkxatbo : integer;
  signal aikfylvsd : real;
  signal kyykchhuv : real;
  signal rito : integer;
  signal mimv : real;
begin
  lp : entity work.mab
    port map (obstatmq => mimv, npgt => rito, yc => kyykchhuv);
  ah : entity work.mab
    port map (obstatmq => aikfylvsd, npgt => ywkxatbo, yc => punxj);
  weoc : entity work.mab
    port map (obstatmq => nnhbb, npgt => tfaowdziej, yc => e);
  yndrnz : entity work.mab
    port map (obstatmq => mzqj, npgt => s, yc => bvwcedcoaf);
end zqtarretdg;



-- Seed after: 13094265355752444432,10557070023141912087
