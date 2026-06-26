-- Seed: 9195003963324294269,12011142928354116943

entity xgp is
  port (hvrvn : in character; mljztvoir : inout time; d : out time_vector(1 downto 2));
end xgp;

architecture voqesjvlpp of xgp is
  
begin
  -- Single-driven assignments
  d <= (others => 0 ns);
  mljztvoir <= 2#01.1_1_1_0# fs;
end voqesjvlpp;

entity atfms is
  port (xxztk : in real_vector(2 downto 4); ju : buffer integer);
end atfms;

architecture ncliah of atfms is
  signal ryraof : time_vector(1 downto 2);
  signal gxz : time;
  signal ggubsi : time_vector(1 downto 2);
  signal lrhk : time;
  signal ba : character;
  signal redtve : time_vector(1 downto 2);
  signal ez : time;
  signal kh : character;
begin
  xarjavo : entity work.xgp
    port map (hvrvn => kh, mljztvoir => ez, d => redtve);
  up : entity work.xgp
    port map (hvrvn => ba, mljztvoir => lrhk, d => ggubsi);
  kmrso : entity work.xgp
    port map (hvrvn => kh, mljztvoir => gxz, d => ryraof);
  
  -- Single-driven assignments
  ju <= 34;
  ba <= 'p';
  kh <= 'y';
end ncliah;



-- Seed after: 6700751397421625619,12011142928354116943
