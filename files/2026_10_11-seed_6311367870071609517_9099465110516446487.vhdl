-- Seed: 6311367870071609517,9099465110516446487

entity rsx is
  port (oj : buffer time; gcsznr : out time_vector(4 downto 4); l : in real);
end rsx;

architecture ewomousi of rsx is
  
begin
  -- Single-driven assignments
  gcsznr <= gcsznr;
  oj <= oj;
end ewomousi;

entity ro is
  port (lnri : out integer_vector(1 to 4));
end ro;

architecture poahvu of ro is
  signal qxcn : time_vector(4 downto 4);
  signal bfy : time;
  signal pjfa : real;
  signal kgnxwbhh : time_vector(4 downto 4);
  signal tnr : time;
  signal icpvt : real;
  signal tjsoimhfog : time_vector(4 downto 4);
  signal g : time;
begin
  voqhuzhr : entity work.rsx
    port map (oj => g, gcsznr => tjsoimhfog, l => icpvt);
  fceqtwe : entity work.rsx
    port map (oj => tnr, gcsznr => kgnxwbhh, l => pjfa);
  xuuwq : entity work.rsx
    port map (oj => bfy, gcsznr => qxcn, l => icpvt);
  
  -- Single-driven assignments
  lnri <= (2#0100#, 1_1_2_2_4, 2#01100#, 16#8#);
  icpvt <= 2#11.1_1_1_1#;
  pjfa <= icpvt;
end poahvu;



-- Seed after: 3115681333868993650,9099465110516446487
