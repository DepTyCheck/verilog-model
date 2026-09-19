-- Seed: 12533066137142530943,14141408946471626091

entity zujbwmz is
  port (ykbhonnkiu : in boolean; whxxmora : in integer; t : buffer time; wcuxa : in boolean);
end zujbwmz;

architecture tadrmpz of zujbwmz is
  
begin
  
end tadrmpz;

entity aiwbbiq is
  port (zjmxgb : linkage real; jugff : out time);
end aiwbbiq;

architecture fwxorzts of aiwbbiq is
  
begin
  -- Single-driven assignments
  jugff <= 0_1_3_3_4.2000 ns;
end fwxorzts;

entity ioodyyoven is
  port (kz : buffer severity_level);
end ioodyyoven;

architecture rpati of ioodyyoven is
  signal k : time;
  signal igwjcea : time;
  signal ixhmwk : real;
  signal qvrmwtwpxi : time;
  signal vutbanboim : boolean;
  signal yfszt : time;
  signal pdryyp : integer;
  signal lqzakxz : boolean;
begin
  nxxfzo : entity work.zujbwmz
    port map (ykbhonnkiu => lqzakxz, whxxmora => pdryyp, t => yfszt, wcuxa => lqzakxz);
  rlvrpv : entity work.zujbwmz
    port map (ykbhonnkiu => vutbanboim, whxxmora => pdryyp, t => qvrmwtwpxi, wcuxa => lqzakxz);
  sljxfs : entity work.aiwbbiq
    port map (zjmxgb => ixhmwk, jugff => igwjcea);
  tw : entity work.zujbwmz
    port map (ykbhonnkiu => vutbanboim, whxxmora => pdryyp, t => k, wcuxa => lqzakxz);
  
  -- Single-driven assignments
  kz <= ERROR;
end rpati;



-- Seed after: 8043387298163008247,14141408946471626091
