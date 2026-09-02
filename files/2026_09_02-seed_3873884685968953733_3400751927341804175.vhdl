-- Seed: 3873884685968953733,3400751927341804175

entity qnaakyzn is
  port (zsqvgz : in bit_vector(3 to 4));
end qnaakyzn;

architecture hftbeofn of qnaakyzn is
  
begin
  
end hftbeofn;

entity bff is
  port (xjyjdcfs : inout bit);
end bff;

architecture dahokhq of bff is
  signal absbxmc : bit_vector(3 to 4);
  signal cdjngsdla : bit_vector(3 to 4);
begin
  nkkf : entity work.qnaakyzn
    port map (zsqvgz => cdjngsdla);
  g : entity work.qnaakyzn
    port map (zsqvgz => cdjngsdla);
  bmh : entity work.qnaakyzn
    port map (zsqvgz => absbxmc);
  yatc : entity work.qnaakyzn
    port map (zsqvgz => cdjngsdla);
  
  -- Single-driven assignments
  cdjngsdla <= ('0', '0');
  absbxmc <= absbxmc;
  xjyjdcfs <= xjyjdcfs;
end dahokhq;



-- Seed after: 1703320337354398919,3400751927341804175
