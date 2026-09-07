-- Seed: 3673571290106336065,12269339630485015285

entity v is
  port (vzsgfvrh : linkage severity_level; bdy : buffer bit);
end v;

architecture vbppa of v is
  
begin
  -- Single-driven assignments
  bdy <= '0';
end vbppa;

entity i is
  port (qbpuc : out time_vector(3 downto 0); pigatxsjl : buffer severity_level; ccaplljwdk : buffer bit);
end i;

architecture vtlgjgu of i is
  
begin
  -- Single-driven assignments
  pigatxsjl <= ERROR;
end vtlgjgu;

entity klmoscfm is
  port (ktfqcnhd : inout integer; bal : inout real; ygqrt : out real; wyetvqxbsu : buffer bit);
end klmoscfm;

architecture nuh of klmoscfm is
  signal ruogbojhb : bit;
  signal mzgml : severity_level;
  signal b : severity_level;
  signal mr : time_vector(3 downto 0);
  signal hhnwyyob : bit;
  signal poufvnjx : severity_level;
  signal ob : time_vector(3 downto 0);
begin
  svcrgvzrdq : entity work.i
    port map (qbpuc => ob, pigatxsjl => poufvnjx, ccaplljwdk => hhnwyyob);
  cwfnfekj : entity work.i
    port map (qbpuc => mr, pigatxsjl => b, ccaplljwdk => wyetvqxbsu);
  esqsmbrtd : entity work.v
    port map (vzsgfvrh => mzgml, bdy => ruogbojhb);
  
  -- Single-driven assignments
  ygqrt <= ygqrt;
  bal <= ygqrt;
end nuh;



-- Seed after: 5169340606726134810,12269339630485015285
