-- Seed: 13029912919060264552,15306146742274979323

entity clqirv is
  port (psi : buffer boolean_vector(4 to 0); kcqlph : linkage bit_vector(4 downto 4));
end clqirv;

architecture tk of clqirv is
  
begin
  -- Single-driven assignments
  psi <= (others => TRUE);
end tk;

entity hefvliywvg is
  port (hq : out real);
end hefvliywvg;

architecture ygseu of hefvliywvg is
  signal bze : bit_vector(4 downto 4);
  signal tceeb : boolean_vector(4 to 0);
  signal xafi : bit_vector(4 downto 4);
  signal mwf : boolean_vector(4 to 0);
  signal d : bit_vector(4 downto 4);
  signal bltjy : boolean_vector(4 to 0);
  signal jqbel : bit_vector(4 downto 4);
  signal vofjkc : boolean_vector(4 to 0);
begin
  eym : entity work.clqirv
    port map (psi => vofjkc, kcqlph => jqbel);
  ppddu : entity work.clqirv
    port map (psi => bltjy, kcqlph => d);
  sgyljb : entity work.clqirv
    port map (psi => mwf, kcqlph => xafi);
  cppwybv : entity work.clqirv
    port map (psi => tceeb, kcqlph => bze);
  
  -- Single-driven assignments
  hq <= 8#0.3#;
end ygseu;



-- Seed after: 9438230093888743142,15306146742274979323
