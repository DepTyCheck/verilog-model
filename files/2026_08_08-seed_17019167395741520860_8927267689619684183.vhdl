-- Seed: 17019167395741520860,8927267689619684183

entity kpkmapni is
  port (vzayymof : buffer integer; itjxkhou : buffer boolean_vector(2 to 0); ts : out bit_vector(0 to 4); adkm : linkage real);
end kpkmapni;

architecture bs of kpkmapni is
  
begin
  -- Single-driven assignments
  ts <= ('1', '1', '0', '0', '0');
end bs;

entity ais is
  port (emwjlbaohl : in integer; fhb : buffer real);
end ais;

architecture vlegaxvrb of ais is
  signal qbnfdmdty : real;
  signal qruabcleki : bit_vector(0 to 4);
  signal db : boolean_vector(2 to 0);
  signal kl : integer;
  signal oabdeg : real;
  signal tqnamakgu : bit_vector(0 to 4);
  signal zibsl : boolean_vector(2 to 0);
  signal vxvedc : integer;
  signal n : bit_vector(0 to 4);
  signal xlsiu : boolean_vector(2 to 0);
  signal rsnij : integer;
begin
  hsouw : entity work.kpkmapni
    port map (vzayymof => rsnij, itjxkhou => xlsiu, ts => n, adkm => fhb);
  ebvzpqtit : entity work.kpkmapni
    port map (vzayymof => vxvedc, itjxkhou => zibsl, ts => tqnamakgu, adkm => oabdeg);
  bsadkk : entity work.kpkmapni
    port map (vzayymof => kl, itjxkhou => db, ts => qruabcleki, adkm => qbnfdmdty);
end vlegaxvrb;



-- Seed after: 9356495769097995278,8927267689619684183
