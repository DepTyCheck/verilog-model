-- Seed: 1782564008320543632,5444032936641997477

entity slrlsku is
  port (bsdxaptnx : linkage boolean_vector(2 downto 0); mwcyytuod : out time_vector(2 downto 2));
end slrlsku;

architecture ukw of slrlsku is
  
begin
  
end ukw;

entity mmowxaf is
  port (zgcbvd : inout integer);
end mmowxaf;

architecture uqfrmpmor of mmowxaf is
  signal a : time_vector(2 downto 2);
  signal pqcqkbsunh : boolean_vector(2 downto 0);
  signal tub : time_vector(2 downto 2);
  signal lpt : boolean_vector(2 downto 0);
begin
  gdamnusfe : entity work.slrlsku
    port map (bsdxaptnx => lpt, mwcyytuod => tub);
  ttjooncn : entity work.slrlsku
    port map (bsdxaptnx => pqcqkbsunh, mwcyytuod => a);
  
  -- Single-driven assignments
  zgcbvd <= zgcbvd;
end uqfrmpmor;



-- Seed after: 4286107069429456068,5444032936641997477
