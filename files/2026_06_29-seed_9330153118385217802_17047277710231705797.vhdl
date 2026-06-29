-- Seed: 9330153118385217802,17047277710231705797

entity aomw is
  port (gkbtn : buffer character);
end aomw;

architecture r of aomw is
  
begin
  -- Single-driven assignments
  gkbtn <= 'k';
end r;

entity iupxxhoaot is
  port (srwj : in real; oo : buffer bit_vector(3 to 0); tipb : out integer);
end iupxxhoaot;

architecture xrmk of iupxxhoaot is
  signal vlzcq : character;
  signal zr : character;
  signal ht : character;
  signal xveluffh : character;
begin
  njfog : entity work.aomw
    port map (gkbtn => xveluffh);
  vj : entity work.aomw
    port map (gkbtn => ht);
  kvcm : entity work.aomw
    port map (gkbtn => zr);
  g : entity work.aomw
    port map (gkbtn => vlzcq);
  
  -- Single-driven assignments
  oo <= (others => '0');
  tipb <= 14;
end xrmk;

entity uwnlqclifu is
  port (jc : in real; pylffypj : out time);
end uwnlqclifu;

architecture xolneud of uwnlqclifu is
  signal fqxc : character;
  signal vdn : character;
  signal xcdrduike : integer;
  signal bqsg : bit_vector(3 to 0);
  signal tl : real;
  signal brq : character;
begin
  agpdbqfuh : entity work.aomw
    port map (gkbtn => brq);
  chvekfeu : entity work.iupxxhoaot
    port map (srwj => tl, oo => bqsg, tipb => xcdrduike);
  w : entity work.aomw
    port map (gkbtn => vdn);
  g : entity work.aomw
    port map (gkbtn => fqxc);
  
  -- Single-driven assignments
  pylffypj <= 2 fs;
  tl <= 23.2044;
end xolneud;



-- Seed after: 8295149806718968877,17047277710231705797
