-- Seed: 7430308234742742918,5444032936641997477

entity jiebu is
  port (zgc : out integer; mzuiwg : buffer time_vector(1 to 0); rvxmdnm : inout boolean; ldftetwsat : buffer bit_vector(1 to 2));
end jiebu;

architecture torzfo of jiebu is
  
begin
  -- Single-driven assignments
  ldftetwsat <= ('1', '0');
end torzfo;

entity td is
  port (doku : buffer boolean; l : linkage bit; hpka : out time);
end td;

architecture ktitui of td is
  signal jfbimcilto : bit_vector(1 to 2);
  signal op : time_vector(1 to 0);
  signal bxexl : integer;
  signal uyeypqvr : bit_vector(1 to 2);
  signal hwxu : boolean;
  signal eblqa : time_vector(1 to 0);
  signal y : integer;
begin
  k : entity work.jiebu
    port map (zgc => y, mzuiwg => eblqa, rvxmdnm => hwxu, ldftetwsat => uyeypqvr);
  ltzlgzrn : entity work.jiebu
    port map (zgc => bxexl, mzuiwg => op, rvxmdnm => doku, ldftetwsat => jfbimcilto);
end ktitui;



-- Seed after: 4761666687750597484,5444032936641997477
