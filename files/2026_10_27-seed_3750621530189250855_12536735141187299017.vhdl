-- Seed: 3750621530189250855,12536735141187299017

entity tn is
  port (wpfm : buffer boolean_vector(0 downto 4));
end tn;

architecture kqsggt of tn is
  
begin
  
end kqsggt;

entity jdg is
  port (rc : buffer integer);
end jdg;

architecture mbpqxan of jdg is
  
begin
  -- Single-driven assignments
  rc <= 4;
end mbpqxan;

entity zej is
  port (apb : linkage integer_vector(3 downto 4));
end zej;

architecture zsagm of zej is
  signal ysaknbv : boolean_vector(0 downto 4);
  signal uaofszj : integer;
  signal dunovn : boolean_vector(0 downto 4);
begin
  ozikflern : entity work.tn
    port map (wpfm => dunovn);
  oozon : entity work.jdg
    port map (rc => uaofszj);
  d : entity work.tn
    port map (wpfm => ysaknbv);
end zsagm;



-- Seed after: 18280346068005527937,12536735141187299017
