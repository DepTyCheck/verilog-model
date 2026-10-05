-- Seed: 15642196918496296585,7304262412290825129

entity mrn is
  port (qyyc : in boolean_vector(0 to 4); ymvstrtslo : inout bit_vector(0 downto 0));
end mrn;

architecture r of mrn is
  
begin
  -- Single-driven assignments
  ymvstrtslo <= ymvstrtslo;
end r;

entity sgofblxy is
  port (idl : buffer integer_vector(1 downto 4));
end sgofblxy;

architecture jx of sgofblxy is
  signal vrwn : bit_vector(0 downto 0);
  signal peu : bit_vector(0 downto 0);
  signal tmry : boolean_vector(0 to 4);
  signal dogt : bit_vector(0 downto 0);
  signal miegoexofl : boolean_vector(0 to 4);
  signal a : bit_vector(0 downto 0);
  signal hhpbm : boolean_vector(0 to 4);
begin
  qtusyjrnnm : entity work.mrn
    port map (qyyc => hhpbm, ymvstrtslo => a);
  hjfyej : entity work.mrn
    port map (qyyc => miegoexofl, ymvstrtslo => dogt);
  gicct : entity work.mrn
    port map (qyyc => tmry, ymvstrtslo => peu);
  tongia : entity work.mrn
    port map (qyyc => hhpbm, ymvstrtslo => vrwn);
  
  -- Single-driven assignments
  idl <= (others => 0);
end jx;

entity ul is
  port (ifgihqvz : buffer bit_vector(3 to 4));
end ul;

architecture vdtad of ul is
  
begin
  -- Single-driven assignments
  ifgihqvz <= ('1', '0');
end vdtad;



-- Seed after: 14375877445187635455,7304262412290825129
