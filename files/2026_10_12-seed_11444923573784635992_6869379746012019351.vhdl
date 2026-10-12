-- Seed: 11444923573784635992,6869379746012019351

entity jcdtds is
  port (xzygvunt : buffer string(5 to 2); qpyummyk : buffer boolean; tasjnqag : inout boolean_vector(2 downto 0); bsvkzuez : in integer_vector(3 to 2));
end jcdtds;

architecture h of jcdtds is
  
begin
  -- Single-driven assignments
  tasjnqag <= (TRUE, FALSE, FALSE);
  xzygvunt <= xzygvunt;
  qpyummyk <= qpyummyk;
end h;

entity noepvnx is
  port (hfzuylx : inout integer);
end noepvnx;

architecture jl of noepvnx is
  
begin
  -- Single-driven assignments
  hfzuylx <= hfzuylx;
end jl;

entity vnqkx is
  port (xjr : out time; ukktizrwd : inout time; xnyft : in time_vector(4 to 3));
end vnqkx;

architecture puidb of vnqkx is
  signal l : integer_vector(3 to 2);
  signal urosabcxii : boolean_vector(2 downto 0);
  signal kbhpttkl : boolean;
  signal jziactvo : string(5 to 2);
begin
  hoscce : entity work.jcdtds
    port map (xzygvunt => jziactvo, qpyummyk => kbhpttkl, tasjnqag => urosabcxii, bsvkzuez => l);
  
  -- Single-driven assignments
  ukktizrwd <= 2_2_4 ms;
  l <= (others => 0);
  xjr <= 4_4.01403 fs;
end puidb;



-- Seed after: 15686994373088254692,6869379746012019351
