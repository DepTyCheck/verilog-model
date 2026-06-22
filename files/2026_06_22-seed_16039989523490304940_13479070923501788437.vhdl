-- Seed: 16039989523490304940,13479070923501788437

entity frlwchdy is
  port (eph : in severity_level; adtirpaaz : inout real);
end frlwchdy;

architecture ubstopn of frlwchdy is
  
begin
  
end ubstopn;

entity hfjidi is
  port (fus : out time; tevuzmjfut : inout real; ylj : buffer time; dvzi : buffer real);
end hfjidi;

architecture zhwndkfy of hfjidi is
  signal ehjfvtule : severity_level;
  signal toinnexr : severity_level;
  signal pus : real;
  signal hbk : severity_level;
begin
  kkrimalzey : entity work.frlwchdy
    port map (eph => hbk, adtirpaaz => pus);
  rlf : entity work.frlwchdy
    port map (eph => toinnexr, adtirpaaz => dvzi);
  faxvhom : entity work.frlwchdy
    port map (eph => ehjfvtule, adtirpaaz => tevuzmjfut);
  
  -- Single-driven assignments
  ylj <= 0133.14 ns;
end zhwndkfy;



-- Seed after: 9302258738323212055,13479070923501788437
