-- Seed: 14758428465964896,9125939553767483053



entity hddvkt is
  port (obkahnr : in time);
end hddvkt;



architecture m of hddvkt is
  
begin
  
end m;



entity tbhoq is
  port (ntbaw : in time; tiydvntvtr : buffer real);
end tbhoq;



architecture jnqk of tbhoq is
  signal uvxkv : time;
  signal tdgwe : time;
begin
  deu : entity work.hddvkt
    port map (obkahnr => ntbaw);
  mh : entity work.hddvkt
    port map (obkahnr => tdgwe);
  gihfrg : entity work.hddvkt
    port map (obkahnr => tdgwe);
  nhtvnawor : entity work.hddvkt
    port map (obkahnr => uvxkv);
end jnqk;



entity xgwqhrm is
  port (moervdsz : inout integer_vector(4 downto 3); uypdhme : linkage bit; qjhauzw : out real; pp : in severity_level);
end xgwqhrm;



architecture flhnkuh of xgwqhrm is
  signal fm : time;
begin
  jhuldr : entity work.hddvkt
    port map (obkahnr => fm);
  djfgwpaxb : entity work.tbhoq
    port map (ntbaw => fm, tiydvntvtr => qjhauzw);
end flhnkuh;



-- Seed after: 1920767026598374135,9125939553767483053
