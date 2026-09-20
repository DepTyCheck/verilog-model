-- Seed: 7547168413018590527,18037650846010261179

entity f is
  port (deks : inout real_vector(4 downto 1); kat : in string(1 downto 5); pibuv : out time);
end f;

architecture kyrrptspfs of f is
  
begin
  -- Single-driven assignments
  deks <= (44.414, 2#1_1_1.0_0_1#, 1.1, 2.3_1_0_1_0);
  pibuv <= pibuv;
end kyrrptspfs;

entity excnodd is
  port (kisitd : buffer time);
end excnodd;

architecture altyiy of excnodd is
  signal ipkpm : time;
  signal cva : string(1 downto 5);
  signal vyojrzzgv : real_vector(4 downto 1);
  signal fvhwbnog : string(1 downto 5);
  signal mxoafvpo : real_vector(4 downto 1);
begin
  luclvyr : entity work.f
    port map (deks => mxoafvpo, kat => fvhwbnog, pibuv => kisitd);
  ru : entity work.f
    port map (deks => vyojrzzgv, kat => cva, pibuv => ipkpm);
  
  -- Single-driven assignments
  fvhwbnog <= "";
end altyiy;



-- Seed after: 17381017278329850665,18037650846010261179
