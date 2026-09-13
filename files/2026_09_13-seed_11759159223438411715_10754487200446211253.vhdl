-- Seed: 11759159223438411715,10754487200446211253

entity vempl is
  port (aswrdp : out severity_level; hcfx : linkage bit_vector(2 to 2); ggsh : buffer integer);
end vempl;

architecture s of vempl is
  
begin
  -- Single-driven assignments
  ggsh <= 2;
  aswrdp <= aswrdp;
end s;

entity fc is
  port (lntso : buffer bit_vector(1 to 4); ijtrxtku : inout integer);
end fc;

architecture mvzzfpl of fc is
  signal mtubpcnw : bit_vector(2 to 2);
  signal oilwbtqabc : severity_level;
  signal ttztd : integer;
  signal afdfltath : bit_vector(2 to 2);
  signal ahmdtsuonb : severity_level;
  signal qo : integer;
  signal ireeqnpjqm : bit_vector(2 to 2);
  signal xwkdci : severity_level;
begin
  aymprhwh : entity work.vempl
    port map (aswrdp => xwkdci, hcfx => ireeqnpjqm, ggsh => qo);
  upil : entity work.vempl
    port map (aswrdp => ahmdtsuonb, hcfx => afdfltath, ggsh => ttztd);
  fbnzg : entity work.vempl
    port map (aswrdp => oilwbtqabc, hcfx => mtubpcnw, ggsh => ijtrxtku);
end mvzzfpl;

entity uomakky is
  port (o : inout integer);
end uomakky;

architecture ap of uomakky is
  signal olckj : integer;
  signal mvwndd : bit_vector(2 to 2);
  signal dwsbhs : severity_level;
  signal pbytvwisie : integer;
  signal rhxeogmqsw : bit_vector(2 to 2);
  signal rqbpzu : severity_level;
begin
  hcjrnzcfo : entity work.vempl
    port map (aswrdp => rqbpzu, hcfx => rhxeogmqsw, ggsh => pbytvwisie);
  wkxcom : entity work.vempl
    port map (aswrdp => dwsbhs, hcfx => mvwndd, ggsh => olckj);
  
  -- Single-driven assignments
  o <= pbytvwisie;
end ap;



-- Seed after: 4608189107423002560,10754487200446211253
