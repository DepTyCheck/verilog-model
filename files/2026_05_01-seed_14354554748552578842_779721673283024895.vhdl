-- Seed: 14354554748552578842,779721673283024895



entity gunst is
  port (zielmbwq : inout severity_level; ul : in severity_level; pechfkm : out time; ldszouw : buffer real);
end gunst;



architecture nuaedbkea of gunst is
  
begin
  
end nuaedbkea;



entity xsqedol is
  port (fpidhf : buffer real; bo : inout time; efehvizrf : inout time; ynij : in real);
end xsqedol;



architecture xwzjfmxb of xsqedol is
  signal ul : real;
  signal qgagjml : severity_level;
  signal hskpetdm : time;
  signal hyv : severity_level;
  signal iauonai : real;
  signal vsz : time;
  signal jnhlgt : severity_level;
  signal zdv : severity_level;
begin
  zxmi : entity work.gunst
    port map (zielmbwq => zdv, ul => jnhlgt, pechfkm => vsz, ldszouw => iauonai);
  dohqicdii : entity work.gunst
    port map (zielmbwq => jnhlgt, ul => hyv, pechfkm => hskpetdm, ldszouw => fpidhf);
  qy : entity work.gunst
    port map (zielmbwq => hyv, ul => qgagjml, pechfkm => bo, ldszouw => ul);
end xwzjfmxb;



-- Seed after: 2566257305790963311,779721673283024895
