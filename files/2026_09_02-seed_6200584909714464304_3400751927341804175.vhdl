-- Seed: 6200584909714464304,3400751927341804175

entity whzh is
  port (mfewxm : in severity_level; qs : out boolean_vector(2 to 2));
end whzh;

architecture cwknmxgq of whzh is
  
begin
  
end cwknmxgq;

entity cnto is
  port (rw : linkage integer);
end cnto;

architecture tjchklkg of cnto is
  signal nroyeynki : boolean_vector(2 to 2);
  signal l : boolean_vector(2 to 2);
  signal vmzwda : severity_level;
  signal kotkkcbl : boolean_vector(2 to 2);
  signal jn : severity_level;
begin
  wdqxx : entity work.whzh
    port map (mfewxm => jn, qs => kotkkcbl);
  ouwegzlvam : entity work.whzh
    port map (mfewxm => vmzwda, qs => l);
  obxnabf : entity work.whzh
    port map (mfewxm => vmzwda, qs => nroyeynki);
  
  -- Single-driven assignments
  jn <= FAILURE;
end tjchklkg;



-- Seed after: 17646839172934007495,3400751927341804175
