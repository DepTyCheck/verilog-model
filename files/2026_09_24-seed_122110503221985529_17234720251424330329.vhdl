-- Seed: 122110503221985529,17234720251424330329

entity rzmruwdax is
  port (vavbwokxb : inout character; ldbewcbtg : buffer integer_vector(4 to 1); bojdlrpqy : buffer boolean_vector(4 to 1));
end rzmruwdax;

architecture lubhspufgb of rzmruwdax is
  
begin
  -- Single-driven assignments
  bojdlrpqy <= (others => TRUE);
end lubhspufgb;

entity vpah is
  port (sjo : buffer real; dpiqrrze : buffer real; altbxna : linkage real);
end vpah;

architecture w of vpah is
  signal wwc : boolean_vector(4 to 1);
  signal irrxo : integer_vector(4 to 1);
  signal ekvgeepdc : character;
  signal qtzikmasdh : boolean_vector(4 to 1);
  signal zknulydz : integer_vector(4 to 1);
  signal wicqt : character;
begin
  srduih : entity work.rzmruwdax
    port map (vavbwokxb => wicqt, ldbewcbtg => zknulydz, bojdlrpqy => qtzikmasdh);
  kt : entity work.rzmruwdax
    port map (vavbwokxb => ekvgeepdc, ldbewcbtg => irrxo, bojdlrpqy => wwc);
  
  -- Single-driven assignments
  dpiqrrze <= 2#0_1_0.1_1_0_0#;
  sjo <= dpiqrrze;
end w;



-- Seed after: 257812821819602648,17234720251424330329
