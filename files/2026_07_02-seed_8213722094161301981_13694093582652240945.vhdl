-- Seed: 8213722094161301981,13694093582652240945

entity fknqn is
  port (zqjqsjd : out boolean; izjakkty : linkage time_vector(0 downto 4); vvjor : out time);
end fknqn;

architecture ocmrpb of fknqn is
  
begin
  -- Single-driven assignments
  vvjor <= 2#10.101# ns;
  zqjqsjd <= TRUE;
end ocmrpb;

entity iu is
  port (zmldtari : buffer time; golm : in bit; shhiu : buffer character);
end iu;

architecture vuky of iu is
  signal px : time;
  signal hdijfzfzs : time_vector(0 downto 4);
  signal qrjcjipit : boolean;
  signal gpylmp : time_vector(0 downto 4);
  signal slq : boolean;
  signal ze : time;
  signal ohphalyre : time_vector(0 downto 4);
  signal vu : boolean;
begin
  u : entity work.fknqn
    port map (zqjqsjd => vu, izjakkty => ohphalyre, vvjor => ze);
  vc : entity work.fknqn
    port map (zqjqsjd => slq, izjakkty => gpylmp, vvjor => zmldtari);
  ojqaj : entity work.fknqn
    port map (zqjqsjd => qrjcjipit, izjakkty => hdijfzfzs, vvjor => px);
  
  -- Single-driven assignments
  shhiu <= 'c';
end vuky;



-- Seed after: 11292618951708171463,13694093582652240945
