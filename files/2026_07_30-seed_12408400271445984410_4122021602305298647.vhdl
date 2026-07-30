-- Seed: 12408400271445984410,4122021602305298647

entity ltvmdi is
  port (vbkaepnn : inout integer_vector(4 downto 3));
end ltvmdi;

architecture dqusbwdiq of ltvmdi is
  
begin
  -- Single-driven assignments
  vbkaepnn <= vbkaepnn;
end dqusbwdiq;

entity srnymdo is
  port (jf : buffer boolean; jyhw : in bit_vector(2 downto 0); ynsetzzvr : out integer);
end srnymdo;

architecture aswarefj of srnymdo is
  signal wywzz : integer_vector(4 downto 3);
begin
  swivrssrtx : entity work.ltvmdi
    port map (vbkaepnn => wywzz);
  
  -- Single-driven assignments
  ynsetzzvr <= ynsetzzvr;
  jf <= jf;
end aswarefj;

entity udmypleue is
  port (irzmtzj : buffer bit; ecvkw : out real);
end udmypleue;

architecture ynqhumpw of udmypleue is
  signal y : integer_vector(4 downto 3);
  signal vfa : integer;
  signal xog : boolean;
  signal msootdfp : integer;
  signal sfyqj : bit_vector(2 downto 0);
  signal ksyxlwxm : boolean;
  signal j : integer;
  signal abrrlupj : bit_vector(2 downto 0);
  signal knneidw : boolean;
begin
  nceczglq : entity work.srnymdo
    port map (jf => knneidw, jyhw => abrrlupj, ynsetzzvr => j);
  wstrapwm : entity work.srnymdo
    port map (jf => ksyxlwxm, jyhw => sfyqj, ynsetzzvr => msootdfp);
  ogqmvousji : entity work.srnymdo
    port map (jf => xog, jyhw => abrrlupj, ynsetzzvr => vfa);
  acqk : entity work.ltvmdi
    port map (vbkaepnn => y);
  
  -- Single-driven assignments
  abrrlupj <= abrrlupj;
end ynqhumpw;



-- Seed after: 11051326777538990268,4122021602305298647
